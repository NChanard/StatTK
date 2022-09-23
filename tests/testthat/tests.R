set.seed(981643)
mat.mtx <- rnorm(10000,50,10)**3 %>% matrix(.,100,100)
BoxBlur(mat.mtx)

set.seed(31415)
x.num <- rnorm(100,50,200)
BreakVector(x.num=x.num, n.num=9)
BreakVector(x.num=x.num, min.num=-400, center.num=58.34, max.num=600, n.num=9, method.chr="linear")
BreakVector(x.num=x.num, center.num=58.34, n.num=9, method.chr="linear")
BreakVector(x.num=x.num, n.num=9, method.chr="density")

set.seed(542972)
x.num <- rnbinom(1000, 1, 0.01)
CbrtNorm(x.num, abs(min(x.num)))

x.num = 1:10
y.num = 1/sqrt(x.num)
elbow = FindElbow(x.num, y.num)

Gauss(x=1)
Gauss(x=1,y=2)

GaussBox( scale.chr="none")
GaussBox( scale.chr="1")
GaussBox( scale.chr="int")

GaussKernel(scale.chr=c("none"))
GaussKernel(scale.chr=c("1"))
GaussKernel(scale.chr=c("int"))

set.seed(20071)
n <- 100
y.num <- sin(seq(0, 5*pi, length.out = n)) + rnorm(n=n, mean = 0, sd=0.1)
y.num[sample(1:n,round(0.5*n))] <- NA
InterpolateNA(y.num,"spline")
InterpolateNA(y.num,"linear")

set.seed(1111)
x.num <- rnorm(1000)
x.num <- sort(x.num)
IqrThreshold(x.num, bounds.chr="lower")
IqrThreshold(x.num, bounds.chr="both")
IqrThreshold(x.num, bounds.chr="upper")

set.seed(542972)
x.num <- rnbinom(1000, 1, 0.01)
Log2Norm(x.num, abs(min(x.num)))


set.seed(542972)
MedianSkewness(rnbinom(10000, 1, 0.01))

set.seed(542972)
x.num <- rnbinom(1000, 1, 0.01)
Normalize(x.num)

Plus(c(1,2,3))
Plus(c(1,2,NA))
Plus(c(NA,NA,NA))


set.seed(1111)
x.num <- 0:100
x.num <- sort(x.num)
QtlThreshold(x.num, prct.num=5, bounds.chr="lower")
QtlThreshold(x.num, prct.num=5, bounds.chr="both")
QtlThreshold(x.num, prct.num=5, bounds.chr="upper")

set.seed(655213)
x.num <- rnorm(500,500)
MinMaxScale(x.num)
MeanScale(x.num)
UnitLengthScale(x.num)
RobustScalarScale(x.num)

set.seed(1111)
x.num <- rnorm(1000)
x.num <- sort(x.num)
x.num
SdThreshold(x.num, sdThreshold.num=2, bounds.chr="lower")
SdThreshold(x.num, sdThreshold.num=2, bounds.chr="both")
SdThreshold(x.num, sdThreshold.num=2, bounds.chr="upper")

set.seed(542972)
x.num <- rnbinom(1000, 1, 0.01)
SqrtNorm(x.num, abs(min(x.num)))

set.seed(1111)
x.num = rnorm(1000)
x.num = sort(x.num)
SdThreshold(x.num)
TrimOutliers(x.num)[990:1000]
TrimOutliers(x.num, clip=TRUE)[990:1000]

