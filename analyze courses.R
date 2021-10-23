install.packages("googlesheets4")
library(googlesheets4)

courseEnrollment <- read_sheet("https://docs.google.com/spreadsheets/d/1Dmzrqk7PiMWsOUzObR5hYyYa38VFEO7kF1c8k-KwzNI/edit#gid=0")

head(courseEnrollment)
courseEnrollment$`Enrollment Count`
sum(courseEnrollment$`Enrollment Count`, na.rm = TRUE)

library(ggplot2)
viscourseEnrollment1 <- ggplot(courseEnrollment, aes(x = `Hub used (To be confirmed)`, y = `Enrollment Count`)) +
  geom_bar(stat =  "identity", na.rm = FALSE) +
  coord_flip() +
  labs(x = "Hub Name",
       y = "Student Enrollment Count",
       caption = "Organizing student count based on datahub") +
  theme(panel.background = element_rect(fill = "NA"),
        axis.line = element_line(color = "black"))

viscourseEnrollment1



viscourseEnrollment2 <- ggplot(courseEnrollment, aes(y = `Hub used (To be confirmed)`, x = `Enrollment Count`)) +
  geom_bar(stat =  "identity", na.rm = FALSE) +
  coord_flip() +
  labs(y = "Hub Name",
       x = "Student Enrollment Count",
       caption = "Organizing student count based on datahub") +
  theme(panel.background = element_rect(fill = "NA"),
        axis.line = element_line(color = "black"))
viscourseEnrollment2

courseEnrollment


library(tidyverse)
courseEnrollment %>% head(arrange(desc(`Enrollment Count`)), n = 10) %>% summarise(mean_count <- sum(`Enrollment Count`))

#Top 10 courses contribute around 5883 students 