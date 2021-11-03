define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  br i1 0, label %l1, label %l2
l1:
  ret i64 49
l2:
  ret i64 8
}

