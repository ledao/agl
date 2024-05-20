let a = 4
let b = 5
let c = ( a + b ) * a + b
print c
let d = a - b
print d
let e = a + ( b + a ) * a
print e
let f = ( a + ( b + a ) ) * a
print f
let g = (a+(b+a))*a
print g

if a > b {
    let c = 10
    print c
    print a
} else {
    print b
}
if 5 > 4 {
    print "5 > 4"
}

if 4 > 5 {
    print "4 > 5"
} else if 3 < 4 {
    print "3 < 4"
} else {
    print "else"
}

struct Point {
    x: i32,
    y: i32
}

let p = Point { x: 10, y: 20 }
print(p.x)
print(p.y)