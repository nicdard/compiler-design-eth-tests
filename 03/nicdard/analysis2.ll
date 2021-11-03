define i64 @main(i64 %argc, i8** %arcv) {
  %1 = mul i64 7, 7
  %2 = add i64 42, %argc
  %3 = alloca i64
  %4 = icmp eq i64 0, %1
  br i1 %4, label %l1, label %l2
l1:
  %5 = bitcast i64* %3 to i8*
  ret i64 %1
l2:
  ret i64 8
}

