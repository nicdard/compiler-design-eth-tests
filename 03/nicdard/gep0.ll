%struct = type { i64, i64, i64 }

@gbl = global %struct { i64 1, i64 10, i64 7 }

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = getelementptr %struct, %struct* @gbl, i32 0, i32 0
  %2 = load i64, i64* %1
  ret i64 %2
}
