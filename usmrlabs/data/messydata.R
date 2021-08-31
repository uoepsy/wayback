require(tidyverse)
clickdat<-readRDS("~/Documents/git_archive/gcd_analysis/processed_data/gcd2_offline.RDS")

#clicks 
clickdat %>% select(subject, trial_no, obj_clicked) %>% 
    mutate(obj_clicked = fct_recode(obj_clicked, "Target"="Referent"),
           subject = paste0("subject_",subject)) %>%
    pivot_wider(names_from=trial_no, values_from=obj_clicked, names_prefix="Trial ") %>%
    #write.csv("data/trialcclicks.csv") %>%
    print

# conditions
clickdat %>% 
    mutate(cond = ifelse(condition=="No Cue", "_fluent.wav","_disfluent.wav"),
           audio = paste0("/files/",ref,cond)) %>%
    select(subject, trial_no, audio, rt) %>% 
    mutate(
        condition = ifelse(grepl("disfluent",audio), 1,0),
        audio = gsub("_disfluent|_fluent","",audio)
    ) -> t

t$rt[sample(1:length(t$rt), size=4)]<-round(rnorm(4,13300,4050))
t$rt[sample(1:length(t$rt), size=4)]<-NA

#write.csv(t,"data/audio.csv",row.names=F)

head(clickdat)
summary(clickdat)


eyedat<-readRDS("~/Documents/git_archive/gcd_analysis/processed_data/gcd2_eyes.RDS")

eyedat %>% 
    filter(CURRENT_BIN>=u_onset_frame) %>% 
    group_by(sub,trial_no,condition) %>%
    summarise(p=mean(ref_fix)) %>%
    group_by(condition) %>%
    summarise(m=mean(p),sd=sd(p))

tempeye <- eyedat %>% 
    filter(CURRENT_BIN>=u_onset_frame) %>% 
    group_by(sub,trial_no,condition) %>%
    summarise(p=mean(ref_fix)) 

tempeye$p[tempeye$condition=="No Cue"] <- rnorm(length(tempeye$p[tempeye$condition=="No Cue"]), 0.542,0.19)
tempeye$p[tempeye$condition=="Adaptor Gesture"] <- rnorm(length(tempeye$p[tempeye$condition=="Adaptor Gesture"]), 0.328,0.16)
tempeye$p <- ifelse(tempeye$p<0, 0, ifelse(tempeye$p > 1, 1, tempeye$p))

ggplot(tempeye, aes(x=p,col=condition))+geom_density()

set.seed(9273)
tempeye %>% select(-condition) %>% rename(subject=sub) %>% 
    left_join(.,t) %>% 
    filter(!is.na(audio)) %>%
    group_by(audio,condition) %>%
    summarise(m = mean(p)) %>%
    mutate(
        m2 = m + rnorm(n(),0,.1)
    ) %>%
    arrange(m2) %>% ungroup %>%
    mutate(
        blinks = rep(1:10,each=4)
    ) -> audioconds

tempeye %>% select(-condition) %>% rename(subject=sub) %>% 
    left_join(.,t) %>% 
    filter(!is.na(audio)) %>% 
    left_join(. , audioconds %>% select(-m,-m2)) %>%
    select(-condition) %>%
    mutate(
        video = paste0("/files/vids/blinks_",blinks,".mp4")
    ) %>% select(-blinks) %>%
    rename(sub=subject) -> blinks

blinks$video[blinks$video=="/files/vids/blinks_8.mp4"]<-"/files/vids/blinsk_8.mp4"

blinks$p[sample(1:nrow(blinks), 6)] <- NA
blinks$p[sample(1:nrow(blinks), 1)] <- -0.2846748321
blinks$p[sample(1:nrow(blinks), 1)] <- 1.4050683827

blinks$distractor_fix = blinks$p * 2000

blinks %>% mutate(rt = ifelse(rt < distractor_fix, rt + 350, rt)) %>%
    mutate(rt = ifelse(rt < distractor_fix, rt + 350, rt)) %>%
    mutate(rt = ifelse(rt < distractor_fix, rt + 350, rt)) -> blinks

write.csv(blinks %>% select(sub,trial_no,distractor_fix,rt),"data/eyegaze.csv",row.names=FALSE)


blinks %>% ungroup %>% select(sub,trial_no,video) %>%
    mutate(
        subject = paste0("subject_",sub)
    ) %>% select(subject,trial_no,video) %>%
    pivot_wider(names_from=trial_no, values_from=video, names_prefix="trial_") %>%
    write.csv(., "data/conditions.csv")

