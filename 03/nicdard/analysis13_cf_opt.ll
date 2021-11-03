define i64 @main(i64 %argc, i8** %arcv) {
  %1 = add i64 0, 0
  %2 = add i64 0, 1
  %3 = add i64 0, 2
  %4 = add i64 0, 3
  %5 = add i64 0, 4
  %6 = add i64 0, 5
  %7 = add i64 0, 7
  %cmp1 = icmp sgt i64 3, 0
  br i1 1, label %one, label %wrong
correct:
  ret i64 7
five:
  %cmp6 = icmp sge i64 10, 5
  br i1 1, label %correct, label %wrong
four:
  %cmp5 = icmp sle i64 4, 10
  br i1 1, label %five, label %wrong
one:
  %cmp2 = icmp eq i64 1, 1
  br i1 1, label %two, label %wrong
three:
  %cmp4 = icmp slt i64 3, 4
  br i1 1, label %four, label %wrong
two:
  %cmp3 = icmp ne i64 2, 3
  br i1 1, label %three, label %wrong
wrong:
  ret i64 0
}

