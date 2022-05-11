; ModuleID = 'Viz'
source_filename = "Viz"

@fmt = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.5 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.6 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@str.7 = private unnamed_addr constant [12 x i8] c"Hello world\00", align 1

declare i32 @printf(i8*, ...)

define void @test_prints(double %x, i32 %y, i8* %z, i1 %h) {
entry:
  %x1 = alloca double, align 8
  store double %x, double* %x1, align 8
  %y2 = alloca i32, align 4
  store i32 %y, i32* %y2, align 4
  %z3 = alloca i8*, align 8
  store i8* %z, i8** %z3, align 8
  %h4 = alloca i1, align 1
  store i1 %h, i1* %h4, align 1
  %x5 = load double, double* %x1, align 8
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), double %x5)
  %y6 = load i32, i32* %y2, align 4
  %printf7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i32 %y6)
  %z8 = load i8*, i8** %z3, align 8
  %printf9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i8* %z8)
  %printf10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i8* getelementptr inbounds ([1 x i8], [1 x i8]* @str, i32 0, i32 0))
  %h11 = load i1, i1* %h4, align 1
  %printf12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.4, i32 0, i32 0), i1 %h11)
  %printf13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.5, i32 0, i32 0), i1 false)
  ret void
}

define void @main() {
entry:
  %b = alloca double, align 8
  store double 0.000000e+00, double* %b, align 8
  store double -1.000000e+01, double* %b, align 8
  %b1 = load double, double* %b, align 8
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.6, i32 0, i32 0), double %b1)
  call void @test_prints(double -1.050000e+01, i32 100, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @str.7, i32 0, i32 0), i1 true)
  ret void
}
