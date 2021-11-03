define i64 @main(i64 %argc, i8** %arcv) {
  %1 = mul i64 7, 7
  %2 = add i64 42, %argc
  %3 = alloca i64
  br label %l1
l1:
  %4 = bitcast i64* %3 to i8*
  ret i64 49
}

