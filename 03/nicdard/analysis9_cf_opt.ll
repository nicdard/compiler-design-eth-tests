define i64 @main(i64 %argc, i8** %argv) {
  %1 = alloca i64
  store i64 5, i64* %1
  %2 = alloca i64
  store i64 1, i64* %2
  %d0 = add i64 0, 1
  br label %guard
body:
  %5 = load i64, i64* %2
  %6 = load i64, i64* %1
  %d2 = add i64 5, 6
  %7 = mul i64 %5, %6
  store i64 %7, i64* %1
  %8 = load i64, i64* %2
  %9 = add i64 %8, 1
  store i64 %9, i64* %2
  br label %guard
end:
  %10 = load i64, i64* %1
  ret i64 %10
guard:
  %3 = load i64, i64* %2
  %4 = icmp sle i64 %3, 10
  %d1 = add i64 1, 4
  br i1 %4, label %body, label %end
}

