define i64 @main(i64 %argc, i8** %argv) {
  %a = add i64 1, 0
  %b = add i64 2, %a
  %c = add i64 3, %b
  %d = add i64 4, %c
  %e = add i64 5, %d
  %f = add i64 6, %e
  %g = add i64 7, %f
  %h = add i64 8, %g
  %i = add i64 9, %h
  %j = add i64 10, %i
  %k = add i64 11, %j
  %l = add i64 12, %k
  %m = add i64 13, %l
  %tmp1 = alloca i64
  store i64 %m, i64* %tmp1
  %tmp2 = load i64, i64* %tmp1
  %n = add i64 14, %tmp2
  %o = add i64 15, %n
  %p = add i64 16, %o
  %q = add i64 17, %p
  %r = add i64 18, %q
  %s = add i64 19, %r
  %t = add i64 20, %s
  %u = add i64 21, %t
  %v = add i64 22, %u
  %w = add i64 23, %v
  %x = add i64 24, %w
  %y = add i64 25, %x
  %z = add i64 26, %y
  ret i64 %z
}

