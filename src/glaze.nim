import std/[macros, macrocache, typetraits, strutils, tables], holo_map/variants

type
  DeglazeFormat* = object
  GlazeFormat* = object
  GlazeError* = object of ValueError

const glazeDumpErrorNode* {.strdefine.} = ""
  ## what way to dump a node on an error
  ## if "repr", uses `repr` 
  ## if "tree", uses `treeRepr`
  ## otherwise, does not dump the node

proc glazeError(msg: string, node: NimNode) =
  when glazeDumpErrorNode == "repr":
    echo repr(node)
  elif glazeDumpErrorNode == "tree":
    echo treeRepr(node)
  # XXX maybe created nodes get line info from stacktrace
  raise newException(GlazeError, node.lineInfo & " " & msg)

proc glazeExpectKind(node: NimNode, kind: NimNodeKind) =
  if node.kind != kind:
    glazeError("expected node of kind " & $kind & ", got " & $node.kind, node)

proc glazeExpectKinds(node: NimNode, kinds: set[NimNodeKind]) =
  if node.kind notin kinds:
    glazeError("expected node of kinds " & $kinds & ", got " & $node.kind, node)

proc glazeExpectLen(node: NimNode, len: int) =
  if node.len != len:
    glazeError("expected node length " & $len & ", got " & $node.kind & " of len " & $node.len, node)

template marshalLitImpl(T) =
  proc marshal*(format: GlazeFormat, val: T): NimNode =
    newLit val

marshalLitImpl int
marshalLitImpl int8
marshalLitImpl int16
marshalLitImpl int32
marshalLitImpl int64

marshalLitImpl uint
marshalLitImpl uint8
marshalLitImpl uint16
marshalLitImpl uint32
marshalLitImpl uint64

marshalLitImpl float
marshalLitImpl float32

marshalLitImpl string

marshalLitImpl char

proc marshal*(format: GlazeFormat, val: cstring): NimNode =
  newCall(bindSym"cstring", newLit $val)

{.push hint[ConvFromXtoItselfNotNeeded]: off.}

template readIntImpl(T) {.dirty.} =
  proc read*(format: DeglazeFormat, node: NimNode, val: var T) =
    glazeExpectKinds node, {nnkIntLit..nnkInt64Lit}
    val = T(node.intVal)

template readUintImpl(T) {.dirty.} =
  proc read*(format: DeglazeFormat, node: NimNode, val: var T) =
    glazeExpectKinds node, {nnkUIntLit..nnkUInt64Lit}
    val = cast[T](node.intVal)

template readFloatImpl(T) {.dirty.} =
  proc read*(format: DeglazeFormat, node: NimNode, val: var T) =
    glazeExpectKinds node, {nnkFloatLit..nnkFloat128Lit}
    val = T(node.floatVal)

readIntImpl int
readIntImpl int8
readIntImpl int16
readIntImpl int32
readIntImpl int64

readUintImpl uint
readUintImpl uint8
readUintImpl uint16
readUintImpl uint32
readUintImpl uint64

readFloatImpl float
readFloatImpl float32

{.pop.}

proc read*(format: DeglazeFormat, node: NimNode, val: var string) =
  glazeExpectKinds node, {nnkStrLit..nnkTripleStrLit}
  val = node.strVal

proc read*(format: DeglazeFormat, node: NimNode, val: var cstring) =
  case node.kind
  of nnkStrLit..nnkTripleStrLit:
    val = cstring(node.strVal)
  of nnkConv, nnkHiddenStdConv, nnkHiddenSubConv, nnkCallKinds:
    expectLen node, 2
    read(format, node[1], val)
  else:
    glazeError "expected cstring but got: " & $node.kind, node

proc read*(format: DeglazeFormat, node: NimNode, val: var char) =
  glazeExpectKinds node, {nnkCharLit..nnkUInt64Lit} # ?
  val = cast[char](node.intVal)

proc marshal*(format: GlazeFormat, val: bool): NimNode =
  result = if val: ident"true" else: ident"false"

proc read*(format: DeglazeFormat, node: NimNode, val: var bool) =
  case node.kind
  of nnkCharLit..nnkUInt64Lit:
    val = bool(node.intVal)
  of nnkStrLit..nnkTripleStrLit:
    val = parseBool(node.strVal)
  of nnkIdent, nnkAccQuoted, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice:
    if node.eqIdent"true":
      val = true
    elif node.eqIdent"false":
      val = false
    else:
      glazeError("expected true or false, got: " & $node, node)
  else:
    glazeError("expected bool, got " & $node.kind, node)

proc marshal*[T: enum](format: GlazeFormat, val: T): NimNode =
  result = newCall(getTypeInst(T), newLit(ord(val)))

macro enumSymbolsCase(T: typedesc, name: string) =
  result = newNimNode(nnkCaseStmt, name)
  result.add name
  var impl = getTypeImpl(T)
  while true:
    if impl.kind in {nnkRefTy, nnkPtrTy, nnkVarTy, nnkOutTy}:
      if impl[^1].kind == nnkObjectTy:
        impl = impl[^1]
      else:
        impl = getTypeInst(impl[^1])
    elif impl.kind == nnkBracketExpr and impl[0].eqIdent"typeDesc":
      impl = getTypeInst(impl[1])
    elif impl.kind == nnkBracketExpr and impl[0].kind == nnkSym:
      impl = getImpl(impl[0])[^1]
    elif impl.kind == nnkSym:
      impl = getImpl(impl)[^1]
    else:
      break
  if impl.kind != nnkEnumTy:
    glazeError "expected enum type for type impl of " & repr(T), impl
  for i in 1 ..< impl.len:
    let field = nimIdentNormalize($impl[i])
    var b = newNimNode(nnkOfBranch, name)
    b.add newLit(field)
    b.add newAssignment(ident"val", impl[i])
    result.add b

proc read*[T: enum](format: DeglazeFormat, node: NimNode, val: var T) =
  case node.kind
  of nnkCharLit..nnkUInt64Lit:
    val = T(node.intVal)
  of nnkStrLit..nnkTripleStrLit:
    val = parseEnum[T](node.strVal)
  of nnkIdent, nnkAccQuoted, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice:
    # getImpl doesn't work for sym
    let name = nimIdentNormalize($node)
    enumSymbolsCase(T, name)
  of nnkConv, nnkHiddenStdConv, nnkHiddenSubConv, nnkCallKinds:
    expectLen node, 2
    read(format, node[1], val)
  else:
    glazeError("expected " & $T & ", got " & $node.kind, node)

proc marshal*[T: distinct](format: GlazeFormat, val: T): NimNode =
  marshal(format, distinctBase(T)(val))

proc read*[T: distinct](format: DeglazeFormat, node: NimNode, val: var T) =
  read(format, node, distinctBase(T)(val))

proc marshal*[I, T](format: GlazeFormat, val: array[I, T]): NimNode =
  result = newNimNode(nnkBracket)
  for a in val:
    result.add marshal(format, a)

proc read*[I, T](format: DeglazeFormat, node: NimNode, val: var array[I, T]) =
  glazeExpectKind node, nnkBracket # nnkTableConstr not supported
  expectLen node, len(val)
  var i = 0
  for a in val.mitems:
    var n = node[i]
    if n.kind == nnkExprColonExpr: n = n[1]
    read(format, n, a)
    inc i

proc marshal*[T](format: GlazeFormat, val: seq[T]): NimNode =
  result = newNimNode(nnkBracket)
  for a in val:
    result.add marshal(format, a)
  result = newCall(bindSym"@", result)

proc read*[T](format: DeglazeFormat, node: NimNode, val: var seq[T]) =
  var node = node
  if node.kind in nnkCallKinds and node[0].eqIdent"@":
    node = node[1]
  glazeExpectKind node, nnkBracket # nnkTableConstr not supported
  val.setLen(node.len)
  for i in 0 ..< node.len:
    var n = node[i]
    if n.kind == nnkExprColonExpr: n = n[1]
    read(format, n, val[i])

proc marshal*[T: tuple](format: GlazeFormat, val: T): NimNode =
  result = newNimNode(nnkTupleConstr)
  for a in val.fields:
    result.add marshal(format, a)

proc read*[T: tuple](format: DeglazeFormat, node: NimNode, val: var T) =
  glazeExpectKinds node, {nnkTupleConstr, nnkPar}
  glazeExpectLen node, arity(T)
  var i = 0
  for a in val.fields:
    var n = node[i]
    if n.kind == nnkExprColonExpr: n = n[1]
    read(format, n, a)
    inc i

proc iterFieldSyms(syms: var Table[string, NimNode], list: NimNode) =
  case list.kind
  of nnkRecList, nnkTupleTy:
    for r in list:
      iterFieldSyms(syms, r)
  of nnkRecCase:
    iterFieldSyms(syms, list[0])
    for bi in 1 ..< list.len:
      glazeExpectKinds list[bi], {nnkOfBranch, nnkElifBranch, nnkElse}
      iterFieldSyms(syms, list[bi][^1])
  of nnkRecWhen:
    for bi in 0 ..< list.len:
      glazeExpectKinds list[bi], {nnkElifBranch, nnkElse}
      iterFieldSyms(syms, list[bi][^1])
  of nnkIdentDefs:
    for i in 0 ..< list.len - 2:
      var sym = list[i]
      if sym.kind == nnkPragmaExpr: sym = sym[0]
      if sym.kind == nnkPostfix: sym = sym[1]
      let name = $sym
      if name notin syms:
        syms[name] = sym
  of nnkSym:
    syms[$list] = list
  of nnkDiscardStmt, nnkNilLit, nnkEmpty: discard
  else:
    glazeError "unknown object field AST kind " & $list.kind, list

proc collectFieldSyms(T: typedesc): Table[string, NimNode] =
  var t = getTypeImpl(T)
  while t != nil:
    # very horribly try to copy macros.customPragma:
    var impl = getTypeImpl(t)
    while true:
      if impl.kind in {nnkRefTy, nnkPtrTy, nnkVarTy, nnkOutTy}:
        if impl[^1].kind == nnkObjectTy:
          impl = impl[^1]
        else:
          impl = getTypeImpl(impl[^1])
      elif impl.kind == nnkBracketExpr and impl[0].eqIdent"typeDesc":
        impl = getTypeImpl(impl[1])
      elif impl.kind == nnkBracketExpr and impl[0].kind == nnkSym:
        impl = getImpl(impl[0])[^1]
      elif impl.kind == nnkSym:
        impl = getImpl(impl)[^1]
      else:
        break
    case impl.kind
    of nnkObjectTy:
      iterFieldSyms(result, impl[^1])
      t = nil
      if impl[1].kind != nnkEmpty:
        expectKind impl[1], nnkOfInherit
        t = impl[1][0]
    else:
      glazeError "got unknown object type kind " & $impl.kind, impl

proc marshal*[T: object | ref object](format: GlazeFormat, val: T): NimNode =
  result = newNimNode(nnkObjConstr)
  result.add getTypeInst(T) #typeToNode(T)
  let syms = collectFieldSyms(T) # XXX cache this
  for name, a in fieldPairs(when val is ref: val[] else: val):
    result.add newTree(nnkExprColonExpr,
      syms[name],
      marshal(format, a))

proc read*[T: object | ref object](format: DeglazeFormat, node: NimNode, val: var T) =
  glazeExpectKind node, nnkObjConstr
  # XXX this is wacky but unfortunately hard to write some other way since
  # static nodes of object variants can contain inaccessible fields
  var fieldNodes: seq[NimNode] = @[] # a table from names to field vals, but iterated manually with `eqIdent`
  for i in 1 ..< node.len:
    let ex = node[i]
    glazeExpectKind ex, nnkExprColonExpr
    fieldNodes.add ex
  when val is ref:
    val = T()
  when hasVariants(T):
    # set variant fields before calling `fieldPairs`
    template onVariantField(f) =
      for fieldNode in fieldNodes.mitems:
        if not fieldNode.isNil and fieldNode[0].eqIdent(astToStr(f)):
          var fval: typeof(val.`f`)
          read(format, fieldNode[1], fval)
          val.`f` = fval
          fieldNode = nil
          break
    withFirstVariantFieldName(T, onVariantField)
  for name, a in fieldPairs(when val is ref: val[] else: val):
    for fieldNode in fieldNodes.mitems:
      if not fieldNode.isNil and fieldNode[0].eqIdent(name):
        var fval: typeof(a)
        read(format, fieldNode[1], fval)
        a = fval
        fieldNode = nil
        break

# hack, grows indefinitely:
const nimNodeLitCache* = CacheSeq"glaze.nimnodelits"

proc getCachedNode(i: int): NimNode = nimNodeLitCache[i]

proc marshal*(format: GlazeFormat, node: NimNode): NimNode =
  let index = len(nimNodeLitCache)
  nimNodeLitCache.add node
  result = newCall(bindSym"getCachedNode", newLit index)

proc read*(format: DeglazeFormat, node: NimNode, val: var NimNode) =
  if node.kind in nnkCallKinds and node.len == 2 and node[0].eqIdent"getCachedNode" and node[1].kind in {nnkIntLit..nnkUInt64Lit}:
    val = nimNodeLitCache[node[1].intVal]
  else:
    glazeError "expected nimnode literal generated by glaze, got " & $node.kind, node

proc glaze*[T](val: T, format = GlazeFormat()): NimNode =
  marshal(format, val)

proc deglaze*[T](node: NimNode, val: var T, format = DeglazeFormat()) =
  read(format, node, val)

proc deglaze*[T](node: NimNode, _: typedesc[T], format = DeglazeFormat()): T =
  read(format, node, result)
