declare void @print()

define void @one(i64 %a) {
    call void @print()
    ret void
}

define void @two(i64 %a,i64 %b) {
    call void @print()
    ret void
}

define void @three(i64 %a,i64 %b,i64 %c) {
    call void @print()
    ret void
}

define void @four(i64 %a,i64 %b,i64 %c,i64 %d) {
    call void @print()
    ret void
}

define void @five(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e) {
    call void @print()
    ret void
}

define void @six(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %f) {
    call void @print()
    ret void
}

define void @seven(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %f,i64 %g) {
    call void @print()
    ret void
}

define void @eight(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %f,i64 %g, i64 %h) {
    call void @print()
    ret void
}

define void @nine(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %f,i64 %g, i64 %h, i64 %i) {
    call void @print()
    ret void
}

define void @ten(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %f,i64 %g, i64 %h, i64 %i, i64 %j) {
    call void @print()
    ret void
}

define void @ll_main() {
    call void @print()
    call void @one(i64 1)
    call void @two(i64 1,i64 1)
    call void @three(i64 1,i64 1,i64 1)
    call void @four(i64 1,i64 1,i64 1,i64 1)
    call void @five(i64 1,i64 1,i64 1,i64 1,i64 1)
    call void @six(i64 1,i64 1,i64 1,i64 1,i64 1,i64 1)
    call void @seven(i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1)
    call void @eight(i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1)
    call void @nine(i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1)
    call void @ten(i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1,i64 1)
    ret void
}