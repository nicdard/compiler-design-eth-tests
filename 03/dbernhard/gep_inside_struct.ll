%var1 = type { i64, i64*, [4 x i64], i64 }

@gint = global i64 42
@gstr = global [11 x i8] c"aabbccddee\00"

@g1 = global %var1 { i64 3, i64* @gint, [4 x i64] [i64 12, i64 199, i64 11, i64 33], i64 42 }


define i64 @main(i64 %argc, i8** %arcv) {
  %1 = getelementptr %var1, %var1* @g1, i32 0, i32 3
  %2 = load i64, i64* %1
  ret i64 %2
}
