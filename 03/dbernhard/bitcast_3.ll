

define i8 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  store i64 24, i64* %1
  %2 = bitcast i64* %1 to i8* 
  %3 = load i8, i8* %2
  ret i8 %3
}

