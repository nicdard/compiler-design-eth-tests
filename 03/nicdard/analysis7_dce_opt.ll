define i64 @main(i64 %argc, i8** %argv) {
  %2 = alloca i64
  %4 = alloca i64
  store i64 1, i64* %4
  br label %guard
body:
  %7 = load i64, i64* %4
  %8 = mul i64 %7, 2
  store i64 %8, i64* %4
  br label %guard
end:
  ret i64 10
guard:
  %5 = load i64, i64* %4
  %6 = icmp slt i64 %5, 10
  br i1 %6, label %body, label %end
}

