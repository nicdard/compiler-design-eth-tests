define i64 @main(i64 %argc, i8** %argv) {
  %1 = alloca i64
  store i64 3, i64* %1
  %2 = alloca i64
  store i64 12, i64* %2
  br i1 1, label %then, label %else
then:
  %3 = load i64, i64* %2
  %4 = sub i64 %3, 10
  store i64 %4, i64* %2
  br label %merge
else:
  %5 = load i64, i64* %2
  %6 = add i64 %5, 10
  store i64 %6, i64* %2
  br label %merge
merge:
  %7 = load i64, i64* %2
  %8 = mul i64 %7, 30
  ret i64 %8
}

