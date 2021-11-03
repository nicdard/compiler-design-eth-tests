define i64 @main(i64 %argc, i8** %argv) {
  br i1 0, label %then, label %else
else:
  br i1 1, label %then, label %merge
merge:
  ret i64 4
then:
  ret i64 2
}

