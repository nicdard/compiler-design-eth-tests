
@gint = global i64 42

define i64* @get_global() {
  ret i64* @gint
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64* @get_global()
  %2 = load i64, i64* %1
  ret i64 %2
}
