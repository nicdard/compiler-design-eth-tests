
@gint = global i64 42

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  %2 = alloca i64

  %3 = load i64, i64* @gint
  store i64 %3, i64* %1
  store i64 10, i64* %2

  call void @swap(i64* %1, i64* %2)
  
  %4 = load i64, i64* %1
  ret i64 %4
}

define void @swap(i64* %0, i64* %1) {
  %3 = load i64, i64* %0
  %4 = load i64, i64* %1
  store i64 %4, i64* %0
  store i64 %3, i64* %1
  ret void
}