define i64 @main(i64 %argc, i8** %argv) {
  %1 = add i64 30, 0
  %2 = sub i64 %1, 24
  %3 = sub i64 9, %2
  %4 = alloca i64
  store i64 %3, i64* %4
  %5 = load i64, i64* %4
  %6 = mul i64 %3, 4
  %7 = alloca i64
  store i64 %6, i64* %7
  %8 = load i64, i64* %7
  %9 = icmp sgt i64 %6, 10
  br i1 %9, label %then, label %else
then:
  %10 = load i64, i64* %7
  %11 = sub i64 %10, 10
  store i64 %11, i64* %7
  br label %merge
else:
  %12 = load i64, i64* %7
  %13 = add i64 %12, 10
  store i64 %13, i64* %7
  br label %merge
merge:
  %14 = load i64, i64* %7
  %15 = sub i64 60, %1
  %16 = mul i64 %14, %15
  ret i64 %16
}
