library(data.table)
library(MASS)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# read in data file from path
data = read.csv("multipleChoiceResponses.csv", header=TRUE, sep = ",")

# remove respondents who did not self identify as male or female due to small sample size
data2 = subset(data, GenderSelect == 'Male' | GenderSelect == 'Female')

# remove respondents below university or over retirement age
data3 = subset(data2, Age > 17 & Age <= 70)

# create top-2 box variables to count only those who use a tool frequently
# syntax generated through string concatenation in Excel
data3$WorkToolsFrequencyAmazonML_t2 = ifelse(data3$WorkToolsFrequencyAmazonML == 'Often' | data3$WorkToolsFrequencyAmazonML == 'Most of the time', 1,0)
data3$WorkToolsFrequencyAWS_t2 = ifelse(data3$WorkToolsFrequencyAWS == 'Often' | data3$WorkToolsFrequencyAWS == 'Most of the time', 1,0)
data3$WorkToolsFrequencyAngoss_t2 = ifelse(data3$WorkToolsFrequencyAngoss == 'Often' | data3$WorkToolsFrequencyAngoss == 'Most of the time', 1,0)
data3$WorkToolsFrequencyC_t2 = ifelse(data3$WorkToolsFrequencyC == 'Often' | data3$WorkToolsFrequencyC == 'Most of the time', 1,0)
data3$WorkToolsFrequencyCloudera_t2 = ifelse(data3$WorkToolsFrequencyCloudera == 'Often' | data3$WorkToolsFrequencyCloudera == 'Most of the time', 1,0)
data3$WorkToolsFrequencyDataRobot_t2 = ifelse(data3$WorkToolsFrequencyDataRobot == 'Often' | data3$WorkToolsFrequencyDataRobot == 'Most of the time', 1,0)
data3$WorkToolsFrequencyFlume_t2 = ifelse(data3$WorkToolsFrequencyFlume == 'Often' | data3$WorkToolsFrequencyFlume == 'Most of the time', 1,0)
data3$WorkToolsFrequencyGCP_t2 = ifelse(data3$WorkToolsFrequencyGCP == 'Often' | data3$WorkToolsFrequencyGCP == 'Most of the time', 1,0)
data3$WorkToolsFrequencyHadoop_t2 = ifelse(data3$WorkToolsFrequencyHadoop == 'Often' | data3$WorkToolsFrequencyHadoop == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMCognos_t2 = ifelse(data3$WorkToolsFrequencyIBMCognos == 'Often' | data3$WorkToolsFrequencyIBMCognos == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMSPSSModeler_t2 = ifelse(data3$WorkToolsFrequencyIBMSPSSModeler == 'Often' | data3$WorkToolsFrequencyIBMSPSSModeler == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMSPSSStatistics_t2 = ifelse(data3$WorkToolsFrequencyIBMSPSSStatistics == 'Often' | data3$WorkToolsFrequencyIBMSPSSStatistics == 'Most of the time', 1,0)
data3$WorkToolsFrequencyIBMWatson_t2 = ifelse(data3$WorkToolsFrequencyIBMWatson == 'Often' | data3$WorkToolsFrequencyIBMWatson == 'Most of the time', 1,0)
data3$WorkToolsFrequencyImpala_t2 = ifelse(data3$WorkToolsFrequencyImpala == 'Often' | data3$WorkToolsFrequencyImpala == 'Most of the time', 1,0)
data3$WorkToolsFrequencyJava_t2 = ifelse(data3$WorkToolsFrequencyJava == 'Often' | data3$WorkToolsFrequencyJava == 'Most of the time', 1,0)
data3$WorkToolsFrequencyJulia_t2 = ifelse(data3$WorkToolsFrequencyJulia == 'Often' | data3$WorkToolsFrequencyJulia == 'Most of the time', 1,0)
data3$WorkToolsFrequencyJupyter_t2 = ifelse(data3$WorkToolsFrequencyJupyter == 'Often' | data3$WorkToolsFrequencyJupyter == 'Most of the time', 1,0)
data3$WorkToolsFrequencyKNIMECommercial_t2 = ifelse(data3$WorkToolsFrequencyKNIMECommercial == 'Often' | data3$WorkToolsFrequencyKNIMECommercial == 'Most of the time', 1,0)
data3$WorkToolsFrequencyKNIMEFree_t2 = ifelse(data3$WorkToolsFrequencyKNIMEFree == 'Often' | data3$WorkToolsFrequencyKNIMEFree == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMathematica_t2 = ifelse(data3$WorkToolsFrequencyMathematica == 'Often' | data3$WorkToolsFrequencyMathematica == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMATLAB_t2 = ifelse(data3$WorkToolsFrequencyMATLAB == 'Often' | data3$WorkToolsFrequencyMATLAB == 'Most of the time', 1,0)
data3$WorkToolsFrequencyAzure_t2 = ifelse(data3$WorkToolsFrequencyAzure == 'Often' | data3$WorkToolsFrequencyAzure == 'Most of the time', 1,0)
data3$WorkToolsFrequencyExcel_t2 = ifelse(data3$WorkToolsFrequencyExcel == 'Often' | data3$WorkToolsFrequencyExcel == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMicrosoftRServer_t2 = ifelse(data3$WorkToolsFrequencyMicrosoftRServer == 'Often' | data3$WorkToolsFrequencyMicrosoftRServer == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMicrosoftSQL_t2 = ifelse(data3$WorkToolsFrequencyMicrosoftSQL == 'Often' | data3$WorkToolsFrequencyMicrosoftSQL == 'Most of the time', 1,0)
data3$WorkToolsFrequencyMinitab_t2 = ifelse(data3$WorkToolsFrequencyMinitab == 'Often' | data3$WorkToolsFrequencyMinitab == 'Most of the time', 1,0)
data3$WorkToolsFrequencyNoSQL_t2 = ifelse(data3$WorkToolsFrequencyNoSQL == 'Often' | data3$WorkToolsFrequencyNoSQL == 'Most of the time', 1,0)
data3$WorkToolsFrequencyOracle_t2 = ifelse(data3$WorkToolsFrequencyOracle == 'Often' | data3$WorkToolsFrequencyOracle == 'Most of the time', 1,0)
data3$WorkToolsFrequencyOrange_t2 = ifelse(data3$WorkToolsFrequencyOrange == 'Often' | data3$WorkToolsFrequencyOrange == 'Most of the time', 1,0)
data3$WorkToolsFrequencyPerl_t2 = ifelse(data3$WorkToolsFrequencyPerl == 'Often' | data3$WorkToolsFrequencyPerl == 'Most of the time', 1,0)
data3$WorkToolsFrequencyPython_t2 = ifelse(data3$WorkToolsFrequencyPython == 'Often' | data3$WorkToolsFrequencyPython == 'Most of the time', 1,0)
data3$WorkToolsFrequencyQlik_t2 = ifelse(data3$WorkToolsFrequencyQlik == 'Often' | data3$WorkToolsFrequencyQlik == 'Most of the time', 1,0)
data3$WorkToolsFrequencyR_t2 = ifelse(data3$WorkToolsFrequencyR == 'Often' | data3$WorkToolsFrequencyR == 'Most of the time', 1,0)
data3$WorkToolsFrequencyRapidMinerCommercial_t2 = ifelse(data3$WorkToolsFrequencyRapidMinerCommercial == 'Often' | data3$WorkToolsFrequencyRapidMinerCommercial == 'Most of the time', 1,0)
data3$WorkToolsFrequencyRapidMinerFree_t2 = ifelse(data3$WorkToolsFrequencyRapidMinerFree == 'Often' | data3$WorkToolsFrequencyRapidMinerFree == 'Most of the time', 1,0)
data3$WorkToolsFrequencySalfrod_t2 = ifelse(data3$WorkToolsFrequencySalfrod == 'Often' | data3$WorkToolsFrequencySalfrod == 'Most of the time', 1,0)
data3$WorkToolsFrequencySAPBusinessObjects_t2 = ifelse(data3$WorkToolsFrequencySAPBusinessObjects == 'Often' | data3$WorkToolsFrequencySAPBusinessObjects == 'Most of the time', 1,0)
data3$WorkToolsFrequencySASBase_t2 = ifelse(data3$WorkToolsFrequencySASBase == 'Often' | data3$WorkToolsFrequencySASBase == 'Most of the time', 1,0)
data3$WorkToolsFrequencySASEnterprise_t2 = ifelse(data3$WorkToolsFrequencySASEnterprise == 'Often' | data3$WorkToolsFrequencySASEnterprise == 'Most of the time', 1,0)
data3$WorkToolsFrequencySASJMP_t2 = ifelse(data3$WorkToolsFrequencySASJMP == 'Often' | data3$WorkToolsFrequencySASJMP == 'Most of the time', 1,0)
data3$WorkToolsFrequencySpark_t2 = ifelse(data3$WorkToolsFrequencySpark == 'Often' | data3$WorkToolsFrequencySpark == 'Most of the time', 1,0)
data3$WorkToolsFrequencySQL_t2 = ifelse(data3$WorkToolsFrequencySQL == 'Often' | data3$WorkToolsFrequencySQL == 'Most of the time', 1,0)
data3$WorkToolsFrequencyStan_t2 = ifelse(data3$WorkToolsFrequencyStan == 'Often' | data3$WorkToolsFrequencyStan == 'Most of the time', 1,0)
data3$WorkToolsFrequencyStatistica_t2 = ifelse(data3$WorkToolsFrequencyStatistica == 'Often' | data3$WorkToolsFrequencyStatistica == 'Most of the time', 1,0)
data3$WorkToolsFrequencyTableau_t2 = ifelse(data3$WorkToolsFrequencyTableau == 'Often' | data3$WorkToolsFrequencyTableau == 'Most of the time', 1,0)
data3$WorkToolsFrequencyTensorFlow_t2 = ifelse(data3$WorkToolsFrequencyTensorFlow == 'Often' | data3$WorkToolsFrequencyTensorFlow == 'Most of the time', 1,0)
data3$WorkToolsFrequencyTIBCO_t2 = ifelse(data3$WorkToolsFrequencyTIBCO == 'Often' | data3$WorkToolsFrequencyTIBCO == 'Most of the time', 1,0)
data3$WorkToolsFrequencyUnix_t2 = ifelse(data3$WorkToolsFrequencyUnix == 'Often' | data3$WorkToolsFrequencyUnix == 'Most of the time', 1,0)

data3$tools = rowSums(data3[229:276])
summary(data3$tools)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.534   3.000  25.000

table(data3$tools, data3$GenderSelect)
#      Female Male 
#0     1609   7085                                                 
#1     203    995                                                 
#2     268    1435                                                 
#3     245    1328                                                 
#4     155    991                                                 
#5     110    647                                                 
#6     51     339                                                 
#7     31     191                                                 
#8     9      122                                                 
#9     7      66                                                 
#10    2      43                                                 
#11    2      22                                                 
#12    0      11                                                 
#13    2      8                                                 
#14    1      4                                                 
#15    1      1                                                 
#16    0      3                                                 
#17    0      3                                                 
#19    0      1                                                 
#21    0      1                                                 
#25    0      1                 

female_t = c(203,268,245,155,110,51,31,9,7,2,2,0,2,1,1,0,0,0,0,0)

male_t = c(995,1435,1328,991,647,339,191,122,66,43,22,11,8,4,1,3,3,1,1,1)


cor.test(female_t, male_t, alternative='two.sided')
#Pearson's product-moment correlation

#data:  female_t and male_t
#t = 43.026, df = 18, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9875588 0.9981320
#sample estimates:
#      cor 
#0.9951735 

# create top-2 box variables to count only those who use a method frequently
# syntax generated through string concatenation in Excel
data3$WorkMethodsFrequencyA/B_t2 = ifelse(data3$WorkMethodsFrequencyA/B == 'Often' | data3$WorkMethodsFrequencyA/B == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyAssociationRules_t2 = ifelse(data3$WorkMethodsFrequencyAssociationRules == 'Often' | data3$WorkMethodsFrequencyAssociationRules == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyBayesian_t2 = ifelse(data3$WorkMethodsFrequencyBayesian == 'Often' | data3$WorkMethodsFrequencyBayesian == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyCNNs_t2 = ifelse(data3$WorkMethodsFrequencyCNNs == 'Often' | data3$WorkMethodsFrequencyCNNs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyCollaborativeFiltering_t2 = ifelse(data3$WorkMethodsFrequencyCollaborativeFiltering == 'Often' | data3$WorkMethodsFrequencyCollaborativeFiltering == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyCross-Validation_t2 = ifelse(data3$WorkMethodsFrequencyCross-Validation == 'Often' | data3$WorkMethodsFrequencyCross-Validation == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyDataVisualization_t2 = ifelse(data3$WorkMethodsFrequencyDataVisualization == 'Often' | data3$WorkMethodsFrequencyDataVisualization == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyDecisionTrees_t2 = ifelse(data3$WorkMethodsFrequencyDecisionTrees == 'Often' | data3$WorkMethodsFrequencyDecisionTrees == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyEnsembleMethods_t2 = ifelse(data3$WorkMethodsFrequencyEnsembleMethods == 'Often' | data3$WorkMethodsFrequencyEnsembleMethods == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyEvolutionaryApproaches_t2 = ifelse(data3$WorkMethodsFrequencyEvolutionaryApproaches == 'Often' | data3$WorkMethodsFrequencyEvolutionaryApproaches == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyGANs_t2 = ifelse(data3$WorkMethodsFrequencyGANs == 'Often' | data3$WorkMethodsFrequencyGANs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyGBM_t2 = ifelse(data3$WorkMethodsFrequencyGBM == 'Often' | data3$WorkMethodsFrequencyGBM == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyHMMs_t2 = ifelse(data3$WorkMethodsFrequencyHMMs == 'Often' | data3$WorkMethodsFrequencyHMMs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyKNN_t2 = ifelse(data3$WorkMethodsFrequencyKNN == 'Often' | data3$WorkMethodsFrequencyKNN == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyLiftAnalysis_t2 = ifelse(data3$WorkMethodsFrequencyLiftAnalysis == 'Often' | data3$WorkMethodsFrequencyLiftAnalysis == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyLogisticRegression_t2 = ifelse(data3$WorkMethodsFrequencyLogisticRegression == 'Often' | data3$WorkMethodsFrequencyLogisticRegression == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyMLN_t2 = ifelse(data3$WorkMethodsFrequencyMLN == 'Often' | data3$WorkMethodsFrequencyMLN == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyNaiveBayes_t2 = ifelse(data3$WorkMethodsFrequencyNaiveBayes == 'Often' | data3$WorkMethodsFrequencyNaiveBayes == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyNLP_t2 = ifelse(data3$WorkMethodsFrequencyNLP == 'Often' | data3$WorkMethodsFrequencyNLP == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyNeuralNetworks_t2 = ifelse(data3$WorkMethodsFrequencyNeuralNetworks == 'Often' | data3$WorkMethodsFrequencyNeuralNetworks == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyPCA_t2 = ifelse(data3$WorkMethodsFrequencyPCA == 'Often' | data3$WorkMethodsFrequencyPCA == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyPrescriptiveModeling_t2 = ifelse(data3$WorkMethodsFrequencyPrescriptiveModeling == 'Often' | data3$WorkMethodsFrequencyPrescriptiveModeling == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyRandomForests_t2 = ifelse(data3$WorkMethodsFrequencyRandomForests == 'Often' | data3$WorkMethodsFrequencyRandomForests == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyRecommenderSystems_t2 = ifelse(data3$WorkMethodsFrequencyRecommenderSystems == 'Often' | data3$WorkMethodsFrequencyRecommenderSystems == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyRNNs_t2 = ifelse(data3$WorkMethodsFrequencyRNNs == 'Often' | data3$WorkMethodsFrequencyRNNs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencySegmentation_t2 = ifelse(data3$WorkMethodsFrequencySegmentation == 'Often' | data3$WorkMethodsFrequencySegmentation == 'Most of the time', 1,0)
data3$WorkMethodsFrequencySimulation_t2 = ifelse(data3$WorkMethodsFrequencySimulation == 'Often' | data3$WorkMethodsFrequencySimulation == 'Most of the time', 1,0)
data3$WorkMethodsFrequencySVMs_t2 = ifelse(data3$WorkMethodsFrequencySVMs == 'Often' | data3$WorkMethodsFrequencySVMs == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyTextAnalysis_t2 = ifelse(data3$WorkMethodsFrequencyTextAnalysis == 'Often' | data3$WorkMethodsFrequencyTextAnalysis == 'Most of the time', 1,0)
data3$WorkMethodsFrequencyTimeSeriesAnalysis_t2 = ifelse(data3$WorkMethodsFrequencyTimeSeriesAnalysis == 'Often' | data3$WorkMethodsFrequencyTimeSeriesAnalysis == 'Most of the time', 1,0)

data3$methods = rowSums(data3[278:305])
summary(data3$methods)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.955   3.000  28.000

table(data3$methods, data3$GenderSelect)
#   Female Male 
#0    1757 7735                                                 
#1     159  747                                                 
#2     141  837                                                 
#3     146  788                                                 
#4     112  734                                                 
#5      91  590                                                 
#6      94  473                                                 
#7      53  333                                                 
#8      50  275                                                 
#9      25  223                                                 
#10     18  155                                                 
#11     14  107                                                 
#12      9   95                                                 
#13      9   55                                                 
#14      7   48                                                 
#15      3   26                                                 
#16      4   27                                                 
#17      1   15                                                 
#18      0   12                                                 
#19      1    6                                                 
#20      0    4                                                 
#21      0    2                                                 
#22      1    3                                                 
#23      0    2                                                 
#24      1    0                                                 
#25      0    1                                                 
#26      0    1                                                 
#28      0    3             

female_m = c(159,141,146,112,91,94,53,50,25,18,14,9,9,7,3,4,1,0,1,0,0,1,0,1,0,0,0)
male_m = c(747,837,788,734,590,473,333,275,223,155,107,95,55,48,26,27,15,12,6,4,2,3,2,0,1,1,3)

cor.test(female_m, male_m, alternative='two.sided')

#Pearson's product-moment correlation
#
#data:  female_m and male_m
#t = 31.099, df = 25, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9719950 0.9942837
#sample estimates:
#      cor 
#0.9873207 

data4 = data3[c('GenderSelect', 'CurrentJobTitleSelect','tools', 'methods')]

males = data4[!(data4$GenderSelect == '' | data4$GenderSelect == 'A different identity'| 
                  data4$GenderSelect == 'Non-binary, genderqueer, or gender non-conforming' |
                  data4$GenderSelect == 'Female'  ), ]

females = data4[!(data4$GenderSelect == '' | data4$GenderSelect == 'A different identity'| 
                    data4$GenderSelect == 'Non-binary, genderqueer, or gender non-conforming' |
                    data4$GenderSelect == 'Male'  ), ]

job_complex_m = aggregate(males[, 3:4], list(males$CurrentJobTitleSelect), FUN="mean")
names(job_complex_m) = c("Title", "Tools_M", "Methods_M")
job_complex_f = aggregate(females[, 3:4], list(females$CurrentJobTitleSelect), FUN="mean")
names(job_complex_f) = c("Title2", "Tools_F", "Methods_F")

job_complex_compare = cbind(job_complex_m, job_complex_f)
job_complex_compare$Job_Labels = str_wrap(job_complex_compare$Title, width=10)


ggplot(job_complex_compare, aes(job_complex_compare$Job_Labels)) + 
  geom_line(aes(y = job_complex_compare$Tools_M, color = '#513f9b',group=1)) + geom_point(y = job_complex_compare$Tools_M) +
  geom_line(aes(y = job_complex_compare$Tools_F, color = '#3f9b75',group=1)) + geom_point(y = job_complex_compare$Tools_F) + 
  theme_minimal() + labs(x="Job Title", y="Average Number of Tools Used", main="Tools by Job and Gender")

ggplot(job_complex_compare, aes(job_complex_compare$Job_Labels)) + 
  geom_line(aes(y = job_complex_compare$Methods_M, color = '#513f9b' , group=1)) + geom_point(y = job_complex_compare$Methods_M) +
  geom_line(aes(y = job_complex_compare$Methods_F, color = '#3f9b75' ,group=1)) + geom_point(y = job_complex_compare$Methods_F) + 
  theme_minimal() + labs(x="Job Title", y="Average Number of Methods Used", main="Methods by Job and Gender")