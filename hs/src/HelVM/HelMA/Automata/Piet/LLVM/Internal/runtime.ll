%Stack = type {
  i32,
  %Stack*
}

@num_formatter = private unnamed_addr constant [3 x i8] c"%d\00"
@stack = private unnamed_addr global %Stack* null


;
; piet commands
;

define void @push(i32 %n) {
  %stack = load %Stack*, %Stack** @stack

  %new_stack_i8 = call i8* @malloc(i32 12)
  %new_stack = bitcast i8* %new_stack_i8 to %Stack*

  %val_ptr = getelementptr inbounds %Stack, %Stack* %new_stack, i32 0, i32 0
  store i32 %n, i32* %val_ptr

  %next_ptr = getelementptr inbounds %Stack, %Stack* %new_stack, i32 0, i32 1
  store %Stack* %stack, %Stack** %next_ptr

  store %Stack* %new_stack, %Stack** @stack
  ret void
}

define void @pop() {
  call void @pop_fp(void (i32)* @pop_pop_op)
  ret void
}

define void @add() {
  call void @pop2_fp(void (i32, i32)* @add_pop2_op)
  ret void
}

define void @subtract() {
  call void @pop2_fp(void (i32, i32)* @subtract_pop2_op)
  ret void
}

define void @multiply() {
  call void @pop2_fp(void (i32, i32)* @multiply_pop2_op)
  ret void
}

define void @divide() {
  call void @pop2_fp(void (i32, i32)* @divide_pop2_op)
  ret void
}

define void @mod() {
  call void @pop2_fp(void (i32, i32)* @mod_pop2_op)
  ret void
}

define void @not() {
  call void @pop_fp(void (i32)* @not_pop_op)
  ret void
}

define void @greater() {
  call void @pop2_fp(void (i32, i32)* @greater_pop2_op)
  ret void
}

define void @pointer(i32* %ref) {
  call void @pop_fp_arg(i32 (i32, i32)* @pointer_pop_op, i32* %ref)
  ret void
}

define void @switch(i32* %ref) {
  call void @pop_fp_arg(i32 (i32, i32)* @switch_pop_op, i32* %ref)
  ret void
}

define void @duplicate() {
  call void @pop_fp(void (i32)* @duplicate_pop_op)
  ret void
}

define void @roll() {
  call void @pop2_fp(void (i32, i32)* @roll_pop2_op)
  ret void
}

define void @in_number() {
  %num_formatter = getelementptr [3 x i8], [3 x i8]* @num_formatter, i32 0, i32 0
  %val_ptr = alloca i32
  %ret = call i32 (i8*, ...) @scanf(i8* %num_formatter, i32* %val_ptr)

  %is_error = icmp sle i32 %ret, 0
  br i1 %is_error, label %error, label %in

in:
  %val = load i32, i32* %val_ptr
  call void @push(i32 %val)
  ret void

error:
  ret void
}

define void @in_char() {
  ; TODO: UNICODE

  %c = call i32 @getchar()

  %is_eof = icmp eq i32 %c, -1
  br i1 %is_eof, label %error, label %in

in:
  call void @push(i32 %c)
  ret void

error:
  ret void
}

define void @out_number() {
  call void @pop_fp(void (i32)* @out_number_pop_op)
  ret void
}

define void @out_char() {
  call void @pop_fp(void (i32)* @out_char_pop_op)
  ret void
}

define void @reset_stack() {
entry:
  %stack = load %Stack*, %Stack** @stack
  br label %loop

loop:
  %cursor = phi %Stack* [%stack, %entry], [%next, %step]

  %end = call i1 @is_empty(%Stack* %cursor)
  br i1 %end, label %exit, label %step

step:
  %cursor_i8 = bitcast %Stack* %cursor to i8*
  call void @free(i8* %cursor_i8)

  %next_ptr = getelementptr inbounds %Stack, %Stack* %cursor, i32 0, i32 1
  %next = load %Stack*, %Stack** %next_ptr

  br label %loop

exit:
  store %Stack* null, %Stack** @stack
  ret void
}


;
; internal higher-order functions
;

define private void @pop_fp(void (i32)* %op) {
  %stack = load %Stack*, %Stack** @stack

  %stack_empty = call i1 @is_empty(%Stack* %stack)
  br i1 %stack_empty, label %error, label %pop

pop:
  %val_ptr = getelementptr inbounds %Stack, %Stack* %stack, i32 0, i32 0
  %val = load i32, i32* %val_ptr
  %next_ptr = getelementptr inbounds %Stack, %Stack* %stack, i32 0, i32 1
  %next = load %Stack*, %Stack** %next_ptr

  %stack_i8 = bitcast %Stack* %stack to i8*
  call void @free(i8* %stack_i8)
  store %Stack* %next, %Stack** @stack

  call void %op(i32 %val)

  ret void

error:
  ret void
}

define private void @pop_fp_arg(i32 (i32, i32)* %op, i32* %ref) {
  %stack = load %Stack*, %Stack** @stack

  %stack_empty = call i1 @is_empty(%Stack* %stack)
  br i1 %stack_empty, label %error, label %pop

pop:
  %val_ptr = getelementptr inbounds %Stack, %Stack* %stack, i32 0, i32 0
  %val = load i32, i32* %val_ptr
  %next_ptr = getelementptr inbounds %Stack, %Stack* %stack, i32 0, i32 1
  %next = load %Stack*, %Stack** %next_ptr

  %stack_i8 = bitcast %Stack* %stack to i8*
  call void @free(i8* %stack_i8)
  store %Stack* %next, %Stack** @stack

  %arg = load i32, i32* %ref
  %res = call i32 %op(i32 %val, i32 %arg)
  store i32 %res, i32* %ref

  ret void

error:
  ret void
}

define private void @pop2_fp(void (i32, i32)* %op) {
  %stack = load %Stack*, %Stack** @stack

  %stack_empty = call i1 @is_empty(%Stack* %stack)
  br i1 %stack_empty, label %error, label %step1

step1:
  %val1_ptr = getelementptr inbounds %Stack, %Stack* %stack, i32 0, i32 0
  %stack1_ptr = getelementptr inbounds %Stack, %Stack* %stack, i32 0, i32 1
  %stack1 = load %Stack*, %Stack** %stack1_ptr

  %stack1_empty = call i1 @is_empty(%Stack* %stack1)
  br i1 %stack1_empty, label %error, label %step2

step2:
  %val2_ptr = getelementptr inbounds %Stack, %Stack* %stack1, i32 0, i32 0
  %stack2_ptr = getelementptr inbounds %Stack, %Stack* %stack1, i32 0, i32 1
  %stack2 = load %Stack*, %Stack** %stack2_ptr

  %val1 = load i32, i32* %val1_ptr
  %val2 = load i32, i32* %val2_ptr

  %stack_i8 = bitcast %Stack* %stack to i8*
  call void @free(i8* %stack_i8)
  %stack1_i8 = bitcast %Stack* %stack1 to i8*
  call void @free(i8* %stack1_i8)
  store %Stack* %stack2, %Stack** @stack

  call void %op(i32 %val2, i32 %val1)

  ret void

error:
  ret void
}

define private void @pop_pop_op(i32) {
  ret void
}

define private void @add_pop2_op(i32 %a, i32 %b) {
  %val = add i32 %a, %b
  call void @push(i32 %val)
  ret void
}

define private void @subtract_pop2_op(i32 %a, i32 %b) {
  %val = sub i32 %a, %b
  call void @push(i32 %val)
  ret void
}

define private void @multiply_pop2_op(i32 %a, i32 %b) {
  %val = mul i32 %a, %b
  call void @push(i32 %val)
  ret void
}

define private void @divide_pop2_op(i32 %a, i32 %b) {
  %zero_div = icmp eq i32 %b, 0
  br i1 %zero_div, label %error, label %div

div:
  %val = call i32 @floored_div(i32 %a, i32 %b)
  call void @push(i32 %val)
  ret void

error:
  call void @push(i32 %a)
  call void @push(i32 %b)
  ret void
}

define private void @mod_pop2_op(i32 %a, i32 %b) {
  %zero_div = icmp eq i32 %b, 0
  br i1 %zero_div, label %error, label %mod

mod:
  %val = call i32 @floored_rem(i32 %a, i32 %b)
  call void @push(i32 %val)
  ret void

error:
  call void @push(i32 %a)
  call void @push(i32 %b)
  ret void
}

define private void @not_pop_op(i32 %n) {
  %is_zero = icmp eq i32 %n, 0
  %val = zext i1 %is_zero to i32
  call void @push(i32 %val)
  ret void
}

define private void @greater_pop2_op(i32 %a, i32 %b) {
  %is_gt = icmp sgt i32 %a, %b
  %val = zext i1 %is_gt to i32
  call void @push(i32 %val)
  ret void
}

define private i32 @pointer_pop_op(i32 %num, i32 %p) {
  %num2 = shl i32 %num, 1
  %new_p_i = add i32 %p, %num2
  %new_p = and i32 %new_p_i, 7
  ret i32 %new_p
}

define private i32 @switch_pop_op(i32 %num, i32 %p) {
  %is_odd = and i32 %num, 1
  %new_p = xor i32 %p, %is_odd
  ret i32 %new_p
}

define private void @duplicate_pop_op(i32 %n) {
  call void @push(i32 %n)
  call void @push(i32 %n)
  ret void
}

define private void @roll_pop2_op(i32 %roll_depth, i32 %roll_num) {
  ; roll(1 2 3 4 5 6 7 8, 5, 2) = 3 4 5 1 2 6 7 8
  ; 1 2 3 4 5 6 7 8
  ; -a- --b-- --c--

  %stack = load %Stack*, %Stack** @stack
  br label %check_depth_zero

check_depth_zero:
  %depth_zero = icmp eq i32 %roll_depth, 0
  br i1 %depth_zero, label %exit, label %check_depth

check_depth:
  %depth_lt_zero = icmp slt i32 %roll_depth, 0
  br i1 %depth_lt_zero, label %error, label %check_num

check_num:
  %roll_num_rem = call i32 @floored_rem(i32 %roll_num, i32 %roll_depth)
  %num_zero = icmp eq i32 %roll_num_rem, 0
  br i1 %num_zero, label %check_size, label %search_init

search_init:
  %a_last_n = sub i32 %roll_num_rem, 1
  %b_last_n = sub i32 %roll_depth, 1
  %a_last_ptr = alloca %Stack*
  %b_last_ptr = alloca %Stack*
  br label %search_loop

search_loop:
  %index = phi i32 [0, %search_init], [%next_index, %search_next]
  %cursor = phi %Stack* [%stack, %search_init], [%next, %search_next]

  %cursor_empty = call i1 @is_empty(%Stack* %cursor)
  br i1 %cursor_empty, label %error, label %search_a

search_a:
  %a_cmp = icmp eq i32 %index, %a_last_n
  br i1 %a_cmp, label %search_found_a, label %search_b

search_b:
  %b_cmp = icmp eq i32 %index, %b_last_n
  br i1 %b_cmp, label %search_found_b, label %search_next

search_found_a:
  store %Stack* %cursor, %Stack** %a_last_ptr
  br label %search_next

search_found_b:
  store %Stack* %cursor, %Stack** %b_last_ptr
  br label %search_end

search_next:
  %next_ptr = getelementptr inbounds %Stack, %Stack* %cursor, i32 0, i32 1
  %next = load %Stack*, %Stack** %next_ptr
  %next_index = add i32 %index, 1
  br label %search_loop

search_end:
  %a_last = load %Stack*, %Stack** %a_last_ptr
  %b_last = load %Stack*, %Stack** %b_last_ptr
  %a_last_next_ptr = getelementptr inbounds %Stack, %Stack* %a_last, i32 0, i32 1
  %b_last_next_ptr = getelementptr inbounds %Stack, %Stack* %b_last, i32 0, i32 1
  %b_first = load %Stack*, %Stack** %a_last_next_ptr
  %c_first = load %Stack*, %Stack** %b_last_next_ptr

  store %Stack* %c_first, %Stack** %a_last_next_ptr
  store %Stack* %stack, %Stack** %b_last_next_ptr

  store %Stack* %b_first, %Stack** @stack
  ret void

check_size:
  %size = call i32 @stack_size(%Stack* %stack)
  %is_overflow = icmp slt i32 %size, %roll_depth
  br i1 %is_overflow, label %error, label %exit

exit:
  ret void

error:
  call void @push(i32 %roll_depth)
  call void @push(i32 %roll_num)
  ret void
}

define private void @out_number_pop_op(i32 %val) {
  %num_formatter = getelementptr [3 x i8], [3 x i8]* @num_formatter, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %num_formatter, i32 %val)
  ret void
}

define private void @out_char_pop_op(i32 %val) {
  ; TODO: UNICODE

  call i32 @putchar(i32 %val)
  ret void
}


;
; internal functions
;

define private i32 @floored_div(i32 %p, i32 %q) {
  %r0 = sdiv i32 %p, %q
  %p1 = mul i32 %q, %r0
  %is_exact = icmp eq i32 %p, %p1
  br i1 %is_exact, label %exact, label %inexact

exact:
  ret i32 %r0

inexact:
  %p_sig = icmp slt i32 %p, 0
  %q_sig = icmp slt i32 %q, 0
  %sig = xor i1 %p_sig, %q_sig
  %sig32 = zext i1 %sig to i32

  %r = sub i32 %r0, %sig32

  ret i32 %r
}

define private i32 @floored_rem(i32 %p, i32 %q) {
  %a = call i32 @floored_div(i32 %p, i32 %q)
  %b = mul i32 %q, %a
  %c = sub i32 %p, %b
  ret i32 %c
}

define private i1 @is_empty(%Stack* %stack) {
  %stack_i64 = ptrtoint %Stack* %stack to i64
  %is_empty = icmp eq i64 %stack_i64, 0
  ret i1 %is_empty
}

define private i32 @stack_size(%Stack* %stack) {
entry:
  br label %loop

loop:
  %size = phi i32 [0, %entry], [%new_size, %step]
  %cursor = phi %Stack* [%stack, %entry], [%next, %step]

  %end = call i1 @is_empty(%Stack* %cursor)
  br i1 %end, label %exit, label %step

step:
  %next_ptr = getelementptr inbounds %Stack, %Stack* %cursor, i32 0, i32 1
  %next = load %Stack*, %Stack** %next_ptr
  %new_size = add i32 %size, 1

  br label %loop

exit:
  ret i32 %size
}


;
; external functions
;

declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i32 @putchar(i32)
declare i32 @getchar()
declare i8* @malloc(i32)
declare void @free(i8*)
