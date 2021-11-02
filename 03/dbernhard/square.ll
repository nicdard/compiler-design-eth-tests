define i64 @square(i64 %0) {
  %2 = alloca i64
  %3 = alloca i64
  store i64 %0, i64* %2
  %4 = load i64, i64* %2
  %5 = load i64, i64* %2
  %6 = mul i64 %4, %5
  store i64 %6, i64* %3
  %7 = load i64, i64* %3
  ret i64 %7
}

define i64 @main(i64 %argc, i8** %argv) {
    %1 = add i64 0, 12
    %2 = call i64 @square(i64 %1)

    ret i64 %2
}