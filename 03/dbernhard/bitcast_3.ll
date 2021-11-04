@gstr = global [11 x i8] c"aabbccddee\00"

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = getelementptr [11 x i8], [11 x i8]* @gstr, i32 0, i32 0
  %2 = bitcast i8* %1 to i64* 
  %3 = load i64, i64* %2
  ret i64 %3
}

