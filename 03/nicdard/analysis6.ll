define i64 @main(i64 %argc, i8** %argv) {
  %1 = add i64 2, 0
  %2 = add i64 3, 0
  %3 = add i64 4, 0
  br i1 0, label %then, label %else
then:
  ret i64 %1
else:
  %4 = sub i64 %3, %2
  br i1 %4, label %then, label %merge
merge:
  ret i64 %3
}
