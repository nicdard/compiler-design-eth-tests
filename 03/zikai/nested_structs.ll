
%struct.ST = type { i64, i64, %struct.RT }
%struct.RT = type { i64, [2 x [4 x i64]], i64 }

@s = global [2 x %struct.ST] [%struct.ST { i64 42, i64 -42, %struct.RT { i64 24, [2 x [4 x i64]] [[4 x i64] [i64 1, i64 2, i64 3, i64 4], [4 x i64] [i64 5, i64 6, i64 7, i64 8]], i64 127 } }, %struct.ST { i64 24, i64 -24, %struct.RT { i64 42, [2 x [4 x i64]] [[4 x i64] [i64 9, i64 10, i64 11, i64 12], [4 x i64] [i64 13, i64 14, i64 15, i64 16]], i64 -127 } }]

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = getelementptr [2 x %struct.ST], [2 x %struct.ST]* @s, i64 0, i64 1, i32 2, i32 1, i64 1, i64 3
  %2 = load i64, i64* %1
  ret i64 %2
}