define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  br label %bar
bar:
  %sa = alloca i64
  %sb = alloca i64
  store i64 14, i64* %sa
  store i64 42, i64* %sb
  br label %foo
foo:
  %v1 = load i64, i64* %sa
  %v2 = load i64, i64* %sa
  %v3 = load i64, i64* %sa
  %v4 = add i64 %v1, %v2
  %res = add i64 %v4, %v3
  ret i64 %res
}

