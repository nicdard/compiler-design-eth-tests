define i64 @main(i64 %argc, i8** %arcv) {
  br i1 1, label %one, label %wrong
correct:
  ret i64 7
five:
  br i1 1, label %correct, label %wrong
four:
  br i1 1, label %five, label %wrong
one:
  br i1 1, label %two, label %wrong
three:
  br i1 1, label %four, label %wrong
two:
  br i1 1, label %three, label %wrong
wrong:
  ret i64 0
}

