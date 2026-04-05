import glaze

type
  Enum = enum
    Enum1, Enum2, Enum3
  Distinct = distinct string

  Foo {.inheritable.} = object
    a: string
    case b: uint8 #range[0..5] # https://github.com/nim-lang/Nim/pull/25585
    of 0..2:
      c: int
      #when true: # nim limitation
      d: bool
    else:
      discard
    notRenamed: Distinct

  Bar = ref object of Foo
    e: seq[tuple[name: string, val: Enum]]

proc `==`(a, b: Distinct): bool {.borrow.}

macro test(): untyped =
  let obj1 = Bar(a: "foo", b: 1, c: 123, d: true, notRenamed: Distinct"bar", e: @{"name1": Enum1, "name3": Enum3, "name2": Enum2})
  let ser = glaze(obj1)
  let deser = deglaze(ser, Bar)
  doAssert obj1.a == deser.a
  doAssert obj1.b == deser.b
  doAssert obj1.c == deser.c
  doAssert obj1.d == deser.d
  doAssert obj1.notRenamed == deser.notRenamed
  doAssert obj1.e == deser.e
  result = ser

let bar = test()
doAssert bar.a == "foo"
doAssert bar.b == 1
doAssert bar.c == 123
doAssert bar.d == true
doAssert bar.notRenamed.string == "bar"
doAssert bar.e == @{"name1": Enum1, "name3": Enum3, "name2": Enum2}
