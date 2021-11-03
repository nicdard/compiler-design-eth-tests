define i64 @main(i64 %argc, i8** %argv) {
  ret i64 3
foo:
  %4 = alloca i64
  store i64 1, i64* %4
  %5 = alloca i64
  store i64 2, i64* %5
  %6 = load i64, i64* %4
  %7 = load i64, i64* %5
  %8 = add i64 %6, %7
  %9 = add i64 %8, 10
  ret i64 %9
}

