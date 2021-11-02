
@gint = global i64 42

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = load i64, i64* @gint
  %2 = add i64 %1, 2
  store i64 %2, i64* @gint
  %3 = load i64, i64* @gint
  ret i64 %3
}
