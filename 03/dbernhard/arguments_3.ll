
define i64 @sum(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7, i64 %x8, i64 %x9, i64 %x10) {
  %1 = add i64 %x1, %x2
  %2 = add i64 %1, %x3
  %3 = add i64 %2, %x4
  %4 = add i64 %3, %x5
  %5 = add i64 %4, %x6
  %6 = add i64 %5, %x7
  %7 = add i64 %6, %x8
  %8 = add i64 %7, %x9
  %9 = add i64 %8, %x10
  ret i64 %9
}

define i64 @ret_last(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7, i64 %x8, i64 %x9, i64 %x10) {
  ret i64 %x10
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = add i64 1, 0
  %2 = add i64 1, %1
  %3 = add i64 1, %2
  %4 = add i64 1, %3
  %5 = add i64 1, %4
  %6 = add i64 1, %5
  %7 = add i64 1, %6
  %8 = add i64 1, %7
  %9 = add i64 1, %8
  %10 = add i64 1, %9

  %11 = call i64 @sum(i64 %1,i64 %2,i64 %3,i64 %4,i64 %5,i64 %6,i64 %7,i64 %8,i64 %9,i64 %10)

  %12 = call i64 @ret_last(i64 %1,i64 %2,i64 %3,i64 %4,i64 %5,i64 %6,i64 %7,i64 %8,i64 %9,i64 %10)

  %13 = add i64 %11, %12
  
  ret i64 %13
}
