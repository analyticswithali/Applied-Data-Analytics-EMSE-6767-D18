##scatter.R
x = c(195,180,220,160,200,220,200,183,139,155)
y = c(130,128,138,122,140,148,142,127,116,123)
#To plot the data in a scatter plot
plot(x, y, pch = 20, cex = 2, main = 'Scatterplot for Cholesterol Level and Systolic Blood Pressure Data', xlab = 'Cholesterol Level', ylab = 'Systolic Blood Pressure')
#To add a trend line
abline(lm(y ~ x), col = 'red')
#To calculate the Pearson correlation coefficient
pearson_cor = cor(x, y)


##pie_chart.R
Freq = c(86, 182, 83, 10)
#To label categories
Process = c('Initial cuto', 'Turning', 'Drilling', 'Assembly')
#To calculate percentages
Percents = round(Freq/sum(Freq)*100,1)
label = paste(Percents, '%', sep=' ') # add % sign to labels
#Pie Chart with percentages
pie(Freq, labels = label, col=c(2,3,4,5), main='Pie Chart of Process Steps')
#To add a legend
legend('topleft', Process, col=c(2,3,4,5), pch=15)








##measures_position.R
x = c(17,12,12,14,15,16,16,16,16,17,17,18,18,18,19,19,20, 20,20,20,20,20,20,20,21,21,21,22,22,23,23,23,24,24,25,26, 26,28,28,28)
#To concatenate resulting mean, standard deviation, variance, and coeficient of variation
 stats = c(mean(x), sd(x), var(x), 100*sd(x)/mean(x))
#To obtain quartiles including min and max
q = quantile(x)
#To obtain the range we find Max-Min
ran = range(x)[2]-range(x)[1]






##histogram.R
SurvTime = c(60,100,130,100,115,30,60,145,75,80,89,57,64,92,87,110,180,195,175,179,159,155,
             146,157,167,174,87,67,73,109,123,135,129,141,154,166,179,37,49,68,74,89,87,109,119,125,56,39,49,190)
#To plot the histogram
hist(SurvTime, breaks=seq(30,198, by=24), main='Histogram of Survival Time', xlab='Survival Time', ylab='Frequency', col='grey', right = FALSE)
#To obtain the cumulative histogram, we replace cell frequencies by their cumulative frequencies
h = hist(SurvTime, breaks=seq(30,198, by=24), right = FALSE)
h$counts = cumsum(h$counts)
#To plot the cumulative histogram
plot(h, main='Cumulative Histogram', xlab='Survival Time', ylab='Cumulative Frequency', col='grey')






##frequency_distribution.R
data =c(4,3,5,3,4,1,2,3,4,3,1,5,3,4,2,1,1,4,5,3,2,5,
        2,5,2,1,2,3,3,2,1,5,3,2,1,1,2,1,2,4,5,3,5,1,3,1,2,1,4,
        1,4,5,4,1,1,2,4,1,4,1,2,4,3,4,1,4,1,4,1,2,1,5,3,1,5,2,
        1,2,3,1,2,2,1,1,2,1,5,3,2,5,5,2,5,3,5,2,3,2,3,5,2,3,5,
        5,2,3,2,5,1,4)

#To get frequencies
data.freq = table(data)
#To combine necessary columns
freq.dist = cbind(data.freq, cumsum(data.freq), 100*cumsum(data.freq)/sum(data.freq))
#To name the table columns
colnames(freq.dist) = c('Frequency','Cum.Frequency','Cum Percentage')
freq.dist






##frequency_distribution_rod.R
#Assign given data to the variable RodData
RodData = c(145,140,120,110,135,150,130,132,137,115,142,115,130,124,139,133,118,127,144 ,143,131,120,117,129,148,130,121,136,133,147,147,128,142,147,152,122,120,145, 126,151)
#To dene the intervals
breaks = seq(110, 152, by=7)
#To assign each observation to its interval
RodData.split = cut(RodData, breaks, right=FALSE)
#To obtain the frequency of data in each class
RodData.freq = table(RodData.split)
#To combine necessary columns
freq.dist = cbind(RodData.freq,100*RodData.freq/ sum(RodData.freq),cumsum(RodData.freq),100*cumsum(RodData.freq )/sum(RodData.freq))
#To name the table columns
colnames(freq.dist) = c('Frequency','Percentage', 'Cum.Frequency','Cum.Percentage')
freq.dist



##boxplots.R
NoiseLevels = c(75,79,80,85,88,89,95,96,97,99,104,105,110,115,140)
#To plot boxplot
boxplot(NoiseLevels, main = 'Box plot of Noise Levels (dB)', ylab = 'Noise Levels (dB)', col ='grey')

boxplot(NoiseLevels, horizontal = TRUE, main = 'Box plot of Noise Levels (dB)', ylab = 'Noise Levels (dB)', col ='grey')






##bar_chart.R

DefectTypes = c(2,1,3,1,2,1,5,4,3,1,2,3,4,3,1,5,2,3,
                1,2,3,5,4,3,1,5,1,4,2,3,2,1,2,5,4,2,4,2,5,1,2,1,2,1,5, 2,1,3,1,4)
#To obtain the frequencies
counts = table(DefectTypes)
#To obtain the bar chart
barplot(counts, xlab='Defect type', ylab='Frequency')
