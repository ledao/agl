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
} else  {
    print "else"
}

struct Point {
    x: i32,
    y: i32
}

let p = Point { x: 20, y: 20 }
print(p.x)
print(p.y)

if p.x > p.y {
    print "p.x > p.y"
} else if p.x == p.y {
    print "p.x == p.y"
} else {
    print "p.x < p.y"
}

fn add(a, b) {
    print "i am in a function"
    return a + b
}

fn sub(a, b) {
    print "i am in sub function"
    return a - b
}

let result = add(5, 3)
print(result)

let result2 = sub(2, 4)
print result2