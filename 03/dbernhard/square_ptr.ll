define void @square(i64* %0) {
  %2 = alloca i64
  %3 = alloca i64

  %4 = load i64, i64* %0
  %5 = load i64, i64* %0

  %6 = mul i64 %4, %5
  store i64* %6, i64* %0
  %7 = load i64, i64* %3
  ret void
}

define i64 @main(i64 %argc, i8** %argv) {
    %1 = alloca i64
    store i64 12, i64* %1
    call i64 @square(i64* %1)
    %2 = load i64, i64* %1
    ret i64 %2
}