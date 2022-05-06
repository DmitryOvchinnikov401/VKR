## Овчинников Дмитрий, Э401



###################################################################
# Список используемых пакетов
install.packages("readxl") #Для чтения Excel
install.packages("stargazer") #Для оформленяи
install.packages("plm") #Для робастных ошибок
install.packages("GGally") #Для матрицы корреляции
install.packages("dplyr") #Для матрицы корреляции
install.packages("car") #Для расчета VIF
install.packages("AER") #Для инструментальных переменных
install.packages("ivpack") #Для 2МНК
install.packages("psych") #Табличка с общей описательной статистикой


# Список используемых библиотек
library("readxl")
library("stargazer") 
library("plm") 
library("GGally") 
library("dplyr") 
library("car") 
library("AER") 
library("ivpack") 
library("psych") 


###################################################################
# Вводим робастные стандартные ошибки

# Функция для обычного МНК 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

# Функция для моделей на панельных данных
clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

# Функция для расчета робастных стандартных ошибок при использовании 2МНК
ivse = function(reg) {
  rob = robust.se(reg)[,2]
  return(rob)
}





###################################################################
# Загружаем и редактируем данные
OrigData <- read_excel("C:\\Users\\Дмитрий\\Desktop\\Диплом\\Данные\\Cross-Section S&P 1200\\Data.xlsx")
Data <- OrigData


# Добавляем переменные
Data$InOwn_2021_Squared=(Data$`InOwn_2021`)^2
Data$StratOwn_2021_Squared=(Data$`StratOwn_2021`)^2
Data$Top10_Squared=(Data$`Top10`)^2
Data$InsiderOwn_Squared=(Data$`InsiderOwn`)^2


###################################################################
# Описательная статистика
summary(Data[,c(36, 38:61)], digits = 2)

## Общая описательная статистика
Data_summary <- describe.by(Data[,c(36, 38, 42, 45:46, 43, 39, 49, 55, 59)], digits = 4)
Data_summary

write.csv(Data_summary, "Data_summary.csv") ## Открыть в редакторе текста



# Таблица с описательными характеристиками по странам
Data_summary_countries <- group_by(Data, "Страны" = Country_of_Headquarters) %>% 
  summarize("N" = n(), "Q" = round(mean(Q), 2), "Size" = round(mean(Size), 2), "Debt" = round(mean(Debt_Ratio), 2), 
            "ROA" = round(mean(ROA), 4), "CapEx" = round(mean(CapEx_to_Rev), 2),  "PP&E" = round(mean(PPE_to_Assets), 2), "Beta" = round(mean(Beta), 2),
            "Institution's share" = round(mean(InOwn_2021), 4), "Strategic entities' share" = round(mean(StratOwn_2021), 4), "Concentration" = round(mean(Top10), 4)) ##Тут надо доработать + выгрузить в CSV
Data_summary_countries

write.csv(Data_summary_countries, "Data_summary_countries.csv", row.names = F) ## Открыть в редакторе текста
# Далее скопировать в Word -> Table-> Convert -> Convert Text to Table. Разделителем указать запятые и проверить число столбцов


# Таблица с описательными характеристиками по отраслям
Data_summary_sectors <- group_by(Data, "Сектора" = GICS_Sector_Name) %>% 
  summarize("N" = n(), "Q" = round(mean(Q), 2), "Size" = round(mean(Size), 2), "Debt" = round(mean(Debt_Ratio), 2), 
            "ROA" = round(mean(ROA), 2), "Beta" = round(mean(Beta), 2),
            "Institution's share" = round(mean(InOwn_2021), 4), "Strategic entities' share" = round(mean(StratOwn_2021), 4), "Concentration" = round(mean(Top10), 4)) ##Тут надо доработать + выгрузить в CSV
Data_summary_sectors

write.csv(Data_summary_sectors, "Data_summary_sectors.csv", row.names = F) ## Открыть в редакторе текста
# Далее скопировать в Word -> Table-> Convert -> Convert Text to Table. Разделителем указать запятые и проверить число столбцов





# Корреляционная матрица для финансовых переменных
Cor_data_1 <- Data[,c(36, 38:40, 42:47)]
cor(Cor_data_1)

Cor_data_1 <- rename(Cor_data_1, "CA" = CA_to_Assets,
                   "PPE" = PPE_to_Assets, "NI" = NI_Margin,
                   "CapEx/Revenue" = CapEx_to_Rev, "FCF" = FCF_to_Rev, 
                   "Debt" = Debt_Ratio, "CapEx" = CapEx_to_Rev)
ggcorr(Cor_data_1, label = TRUE)



###################################################################
# Графики
# Q-тобина
hist(Data$Q, breaks = 20, freq = FALSE, col = "lightblue",
 xlab = "Q-тобина",
 ylab = "Плотность вероятности",
 main = "Гистограмма распределения Q-тобина, совмещенная с кривой плотности")
lines(density(Data$Q), col = "red", lwd = 2)

# Size
hist(Data$Size, breaks = 20, col = "tomato3", border = "white", xlim = c(20,28),
     xlab = "Размер компании (Size)",
     ylab = "Число наблюдений",
     main = "")
lines(density(Data$Size), col = "gray18", lwd = 2)

# InOwn
hist(Data$InOwn_2021, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "Доля институциональных инвесторов",
     ylab = "Плотность вероятности",
     main = "Гистограмма распределения, совмещенная с кривой плотности")
lines(density(Data$InOwn_2021), col = "red", lwd = 2)

# Top-10
hist(Data$Top10, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "Доля 10 крупнейших акционеров",
     ylab = "Плотность вероятности",
     main = "")
lines(density(Data$Top10), col = "red", lwd = 2)





###################################################################
## a) Модели для концентрации
# Без стран и индустрий
model_01a <- lm(data = Data, Q ~ Top10 + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta)
summary(model_01a)


# С индустриями (без RE)
model_02a <- lm(data = Data, Q ~ Top10 + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT)
summary(model_02a)


# Со странами (без BRA)
model_03a <- lm(data = Data, Q ~ Top10 + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                 JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_03a)


# Со странами и индустриями (без RE и USA)
model_04a <- lm(data = Data, Q ~ Top10 + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                 AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                 JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04a)


# Обобщение
stargazer(model_01a, model_02a, model_03a, model_04a,   
          se=list(cse(model_01a),cse(model_02a), cse(model_03a), cse(model_04a)), 
          title="Simple cross-section regressions", type="text", 
          df=FALSE, digits=2, out = "Concentration_models.html", summary = FALSE)






## b) Модели для StratOwn & InOwn
# Без стран и индустрий
model_01b <- lm(data = Data, Q ~ InOwn_2021 + StratOwn_2021 +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta)
summary(model_01b)


# С индустриями (без RE)
model_02b <- lm(data = Data, Q ~ InOwn_2021 + StratOwn_2021 +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT)
summary(model_02b)


# Со странами (без BRA)
model_03b <- lm(data = Data, Q ~ InOwn_2021 + StratOwn_2021 +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                 JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_03b)


# Со странами и индустриями (без RE и BRA)
model_04b <- lm(data = Data, Q ~ InOwn_2021 + StratOwn_2021 +  
                Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta +
                CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04b)


# Обобщение
stargazer(model_01b, model_02b, model_03b, model_04b, 
          se=list(cse(model_01b),cse(model_02b), cse(model_03b), cse(model_04b)), 
          title="Simple cross-section regressions", type="text", 
          df=FALSE, digits=2, out = "Inst_and_Strat_models.html", summary = FALSE)




## c) Модели для разных типов акционеров (Институты, Гос-во, Инсайдеры и Холдинги)
# Без стран и индустрий
model_01c <- lm(data = Data, Q ~ InOwn_2021 + Corp_and_Hold + Individuals + Government + InsiderOwn +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta)
summary(model_01c)


# С индустриями (без RE)
model_02c <- lm(data = Data, Q ~ InOwn_2021 + Corp_and_Hold + Individuals + Government + InsiderOwn +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT)
summary(model_02c)


# Со странами (без BRA)
model_03c <- lm(data = Data, Q ~ InOwn_2021 + Corp_and_Hold + Individuals + Government + InsiderOwn +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_03b)


# Со странами и индустриями (без RE и BRA)
model_04c <- lm(data = Data, Q ~ InOwn_2021 + Corp_and_Hold + Individuals + Government + InsiderOwn +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04c)


# Со странами и индустриями + квадрат инсайдеров (без RE и BRA)
model_05c <- lm(data = Data, Q ~ InOwn_2021 + Corp_and_Hold + Individuals + Government + InsiderOwn + InsiderOwn_Squared +
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_05c)

# Обобщение
stargazer(model_01c, model_02c, model_03c, model_04c, model_05c,   
          se=list(cse(model_01c),cse(model_02c), cse(model_03c), cse(model_04c), cse(model_05c)), 
          title="Simple cross-section regressions", type="text", 
          df=FALSE, digits=2, out = "Inst_and_differentStrat_models.html", summary = FALSE)


###################################################################
## Различные тесты моделей

# 1. Мультиколлинеарность
vif_values <- vif(model_04b)
vif_values
barplot(vif_values[1:12], main = "VIF значения", horiz = TRUE, col = "gray27")
#add vertical line at 5
abline(v = 10, lwd = 3, lty = 2)



# 2. Эндогенность
model_iv <- ivreg(data = Data, Q ~ InOwn_2021 + StratOwn_2021 + 
                    Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                    CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT + 
                    AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                    JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA | InOwn_2020 + StratOwn_2020 +
                    Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                    CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT + 
                    AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                    JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_iv)

# Подробнo по шагам
olsreg_1 <- lm(data = Data, InOwn_2021 ~ InOwn_2020 + StratOwn_2020 + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT + 
                 AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                 JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
IO_Hat <- fitted(olsreg_1)
summary(olsreg_1)


olsreg_2 <- lm(data = Data, StratOwn_2021 ~ InOwn_2020 + StratOwn_2020 + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT + 
                 AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                 JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
SO_Hat <- fitted(olsreg_2)
summary(olsreg_2)

olsreg_3 <- lm(data = Data, Q ~ IO_Hat + SO_Hat + Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta + 
                 CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT + 
                 AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                 JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(olsreg_3)




## Сравнение с обычной моделью
stargazer(model_04b, model_iv,
          se=list(cse(model_04b),ivse(model_iv)), 
          title="Simple cross-section regressions", type="text", 
          df=FALSE, digits=2)

## Обобщение и выводы + тесты для инструментов
summary(model_iv, vcov = sandwich, diagnostics = TRUE)


# 3. Альтернативная зависимая переменная (MTB)

# Со странами и индустриями (без RE и BRA)
model_04f <- lm(data = Data, MTB ~ InOwn_2021 + StratOwn_2021 +  
                  Debt_Ratio + Size + ROA + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04f)

# Альтернативные контрольные переменные: FCF и NI
# NI для Q
model_04g <- lm(data = Data, Q ~ InOwn_2021 + StratOwn_2021 +  
                  Debt_Ratio + Size + NI_Margin + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04g)

# NI для MTB
model_04g1 <- lm(data = Data, MTB ~ InOwn_2021 + StratOwn_2021 +  
                  Debt_Ratio + Size + NI_Margin + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04g1)




#FCF
model_04h <- lm(data = Data, Q ~ InOwn_2021 + StratOwn_2021 +  
                  Debt_Ratio + Size + FCF_to_Rev + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04h)

#FCF для MTB
model_04h1 <- lm(data = Data, MTB ~ InOwn_2021 + StratOwn_2021 +  
                  Debt_Ratio + Size + FCF_to_Rev + CapEx_to_Rev + PPE_to_Assets + Beta +
                  CSR + CDS + CST + ENR + HCR + IND + IT + MAT + UT +
                  AUS + BEL + CAN + CHL + CHN + DNK + FIN + FRA + DEU + HKG + IRL + ITA +
                  JPN + KOR + LUX + NLD + NOR + ESP + SWE + CHE + TWN + GBR + USA)
summary(model_04h1)


## Общая таблица для сравнения устойчивости
stargazer(model_04b, model_04g, model_04h, model_iv, model_04f, model_04g1, model_04h1,
          se=list(cse(model_04b), cse(model_04g), cse(model_04h), ivse(model_iv), cse(model_04f), cse(model_04g1), cse(model_04h1)), 
          title="Simple cross-section regressions", type="text", 
          df=FALSE, digits=2, out = "IV_models.html", summary = FALSE)







