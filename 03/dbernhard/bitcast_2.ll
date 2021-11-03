

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  store i64 24, i64* %1
  %2 = bitcast i64* %1 to i64* 
  %3 = load i64, i64* %2
  ret i64 %3
}

