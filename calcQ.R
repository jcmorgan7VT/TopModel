# 2/18/25
# John Morgan
# script defining function to process salt dilution gauging measurements
#figuring out function

#define function
calcQ <- function(path){

  meta <- read_csv(paste0(path,"/metadata.csv"))
  
  vol <- meta$slugVolume_mL #volume of slug [mL]
  V <- vol * 1e-6 #volume of slug [m^3]
  
  #relative conductivity calculations
  #secondary solution
  X <- 10 #volume of salt slug solution added to 1 L of secondary solution [mL]
  V0 <- 1000 #volume of water in secondary solution [mL]
  RCsec <- X / (V0 + X) #relative cond of secondary solution
  #calibration tank
  k_raw <- read_csv(paste0(path,"/k.csv"))
  #find the conductivity before the first slug added
  k_for_ec <- k_raw %>% 
    slice(max(1, match(TRUE, slugadded_mL > 0) - 6):match(TRUE, slugadded_mL > 0)) %>% 
    filter(row_number() <= n()-1)%>% 
    summarise(ec0 = mean(cond_k))
  
  
  EC0 <- k_for_ec$ec0 #background cond of calibration tank [micro S/cm]
  
  #calculate relative cond at each time step
  #amount of secondary solution added
  Dsec <- 10 # [mL]
  
  #Dsec <- meta$timestep # [mL]
  
  
  k_measurements <- read_csv(paste0(path,"/k.csv")) %>% 
    filter(slugadded_mL == 10) %>% 
    mutate(time = mdy_hm(time)) #%>% 
  #slice for calibration measurements that look good
  # slice(0:timesteps)
  
  timesteps <- length(k_measurements$slugadded_mL)
  
  k_measurements <- k_measurements %>% slice(0:timesteps)
  
  RC <- seq(0, timesteps, 1)
  RC <- (RCsec * RC * Dsec) /(V0 + (RC * Dsec)) #Vc, volume of secs at each timestep
  
  #input actual conductivity at each timestep
  EC <- c(EC0,k_measurements$cond_k)

 # plot(EC, RC)
  
  #calculate k
  #slope of the line between cond and relative cond from calibration tank
  k <- unname(lm(RC~EC)$coefficients[2])
  #back to calculating Q
  DeltaT <- meta$timestep #[s]
  
  #calculate sum of change in conductivity minus background
  #use upstream and downstream cond measurements
  cond <- read_csv(paste0(path,"/cond.csv")) %>% 
    mutate(time_down = mdy_hm(time_down), 
           time_up = mdy_hm(time_up)) 

  if(DeltaT == 10){
#remove unneeded timesteps at the top
if(cond$time_down[1] != cond$time_down[6]){
  cond <- cond[-1,]
}
if(cond$time_down[1] != cond$time_down[6]){
  cond <- cond[-1,]
}
if(cond$time_down[1] != cond$time_down[6]){
  cond <- cond[-1,]
}
if(cond$time_down[1] != cond$time_down[6]){
  cond <- cond[-1,]
}
if(cond$time_down[1] != cond$time_down[6]){
  cond <- cond[-1,]
}
if(cond$time_down[1] != cond$time_down[6]){
  cond <- cond[-1,]
}

#remove unneeded timesteps at the bottom
if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
  cond <- cond[-length(cond$time_down),]
}
if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
  cond <- cond[-length(cond$time_down),]
}
if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
  cond <- cond[-length(cond$time_down),]
}
if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
  cond <- cond[-length(cond$time_down),]
}
if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
  cond <- cond[-length(cond$time_down),]
}
if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
  cond <- cond[-length(cond$time_down),]
  

}
} else if(DeltaT == 30) {
  if(cond$time_down[1] != cond$time_down[2]){
    cond <- cond[-1,]
  }
  if(cond$time_down[1] != cond$time_down[2]){
    cond <- cond[-1,]
  }
  if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 1]){
    cond <- cond[-length(cond$time_down),]
  }
  if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 1]){
    cond <- cond[-length(cond$time_down),]
  }
  
}
  
  if(DeltaT == 10){
  cond <- cond %>% mutate(secs = rep(seq(0,50,10), length(cond$time_down)/6)) %>% 
    mutate(time_down = time_down + second(secs),
           time_up = time_up + second(secs),
           diff = cond_down - cond_up)
  }
  
  if(DeltaT == 30){
    cond <- cond %>% mutate(secs = rep(seq(0,30,30), length(cond$time_down)/2)) %>% 
      mutate(time_down = time_down + second(secs),
             time_up = time_up + second(secs),
             diff = cond_down - cond_up)
  }  
  
  

  #find the sum of the difference between upstream and downstream cond
  sum <- sum(cond$diff)
  #final calculation
  Q <- V / (k * DeltaT * sum) # in mL/s
  Q_Ls <- Q * 1000 # convert to L/s
  return(data.frame("discharge" = Q_Ls,
                    "k" = k,
                    "datetime" = cond$time_down[1]))
}

#function for no k
calcQ_noK <- function(path, k){
  
  meta <- read_csv(paste0(path,"/metadata.csv"))
  
  vol <- meta$slugVolume_mL #volume of slug [mL]
  V <- vol * 1e-6 #volume of slug [m^3]
  
  #relative conductivity calculations
  #secondary solution
  X <- 10 #volume of salt slug solution added to 1 L of secondary solution [mL]
  V0 <- 1000 #volume of water in secondary solution [mL]
  RCsec <- X / (V0 + X) #relative cond of secondary solution
  #calibration tank
  
  
  #k <- unname(lm(RC~EC)$coefficients[2])
  #back to calculating Q
  DeltaT <- meta$timestep #[s]
  
  #calculate sum of change in conductivity minus background
  #use upstream and downstream cond measurements
  cond <- read_csv(paste0(path,"/cond.csv")) %>% 
    mutate(time_down = mdy_hm(time_down), 
           time_up = mdy_hm(time_up)) 
  
  if(DeltaT == 10){
    #remove unneeded timesteps at the top
    if(cond$time_down[1] != cond$time_down[6]){
      cond <- cond[-1,]
    }
    if(cond$time_down[1] != cond$time_down[6]){
      cond <- cond[-1,]
    }
    if(cond$time_down[1] != cond$time_down[6]){
      cond <- cond[-1,]
    }
    if(cond$time_down[1] != cond$time_down[6]){
      cond <- cond[-1,]
    }
    if(cond$time_down[1] != cond$time_down[6]){
      cond <- cond[-1,]
    }
    if(cond$time_down[1] != cond$time_down[6]){
      cond <- cond[-1,]
    }
    
    #remove unneeded timesteps at the bottom
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
      cond <- cond[-length(cond$time_down),]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
      cond <- cond[-length(cond$time_down),]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
      cond <- cond[-length(cond$time_down),]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
      cond <- cond[-length(cond$time_down),]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
      cond <- cond[-length(cond$time_down),]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 5]){
      cond <- cond[-length(cond$time_down),]
      
      
    }
  } else if(DeltaT == 30) {
    if(cond$time_down[1] != cond$time_down[2]){
      cond <- cond[-1,]
    }
    if(cond$time_down[1] != cond$time_down[2]){
      cond <- cond[-1,]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 1]){
      cond <- cond[-length(cond$time_down),]
    }
    if(cond$time_down[length(cond$time_down)] != cond$time_down[length(cond$time_down) - 1]){
      cond <- cond[-length(cond$time_down),]
    }
    
  }
  
  if(DeltaT == 10){
    cond <- cond %>% mutate(secs = rep(seq(0,50,10), length(cond$time_down)/6)) %>% 
      mutate(time_down = time_down + second(secs),
             time_up = time_up + second(secs),
             diff = cond_down - cond_up)
  }
  
  if(DeltaT == 30){
    cond <- cond %>% mutate(secs = rep(seq(0,30,30), length(cond$time_down)/2)) %>% 
      mutate(time_down = time_down + second(secs),
             time_up = time_up + second(secs),
             diff = cond_down - cond_up)
  }  
  
  
  
  #find the sum of the difference between upstream and downstream cond
  sum <- sum(cond$diff)
  #final calculation
  Q <- V / (k * DeltaT * sum)
  Q_Ls <- Q * 1000
  return(data.frame("discharge" = Q_Ls,
                    "k" = k,
                    "datetime" = cond$time_down[1]))
}

#function for no k, only one logger
calcQ_noK_oneL <- function(path, k){
  
  meta <- read_csv(paste0(path,"/metadata.csv"))
  
  vol <- meta$slugVolume_mL #volume of slug [mL]
  V <- vol * 1e-6 #volume of slug [m^3]
  
  #relative conductivity calculations
  #secondary solution
  X <- 10 #volume of salt slug solution added to 1 L of secondary solution [mL]
  V0 <- 1000 #volume of water in secondary solution [mL]
  RCsec <- X / (V0 + X) #relative cond of secondary solution
  #calibration tank
  
  
  #k <- unname(lm(RC~EC)$coefficients[2])
  #back to calculating Q
  DeltaT <- meta$timestep #[s]
  
  #calculate sum of change in conductivity minus background
  #use upstream and downstream cond measurements
  cond <- read_csv(paste0(path,"/cond.csv")) %>% 
    mutate(time_down = mdy_hm(time_down)
    )
  
  start <- cond$time_down[which(cond$plume_status == "start")]
  stop <- cond$time_down[which(cond$plume_status == "stop")]
  
  cond <- 
    cond %>% 
    filter(time_down < stop) %>% 
    filter(time_down >= start) #%>% 
  
  if(DeltaT == 10){
    cond <- cond %>% mutate(secs = rep(seq(0,50,10), length(cond$time_down)/6)) %>% 
      mutate(time_down = time_down + second(secs),
             diff = cond_down - meta$Background_cond)
  }
  
  if(DeltaT == 30){
    cond <- cond %>% mutate(secs = rep(seq(0,30,30), length(cond$time_down)/2)) %>% 
      mutate(time_down = time_down + second(secs),
             diff = cond_down - meta$Background_cond)
  }  
  
  
  
  #find the sum of the difference between upstream and downstream cond
  sum <- sum(cond$diff)
  #final calculation
  Q <- V / (k * DeltaT * sum)
  Q_Ls <- Q * 1000
  return(data.frame("discharge" = Q_Ls,
                    "k" = k,
                    "datetime" = cond$time_down[1]))
}


#function to preview RC calculations
plotK <- function(path){
  
  meta <- read_csv(paste0(path,"/metadata.csv"))
  
  vol <- meta$slugVolume_mL #volume of slug [mL]
  V <- vol * 1e-6 #volume of slug [m^3]
  
  #relative conductivity calculations
  #secondary solution
  X <- 10 #volume of salt slug solution added to 1 L of secondary solution [mL]
  V0 <- 1000 #volume of water in secondary solution [mL]
  RCsec <- X / (V0 + X) #relative cond of secondary solution
  #calibration tank
  k_raw <- read_csv(paste0(path,"/k.csv"))
  #find the conductivity before the first slug added
  k_for_ec <- k_raw %>% 
    slice(max(1, match(TRUE, slugadded_mL > 0) - 6):match(TRUE, slugadded_mL > 0)) %>% 
    filter(row_number() <= n()-1)%>% 
    summarise(ec0 = mean(cond_k))
  
  
  EC0 <- k_for_ec$ec0 #background cond of calibration tank [micro S/cm]
  
  #calculate relative cond at each time step
  #amount of secondary solution added
  Dsec <- 10 # [mL]
  
  #Dsec <- meta$timestep # [mL]
  
  
  k_measurements <- read_csv(paste0(path,"/k.csv")) %>% 
    filter(slugadded_mL == 10) %>% 
    mutate(time = mdy_hm(time)) #%>% 
  #slice for calibration measurements that look good
  # slice(0:timesteps)
  
  timesteps <- length(k_measurements$slugadded_mL)
  
  k_measurements <- k_measurements %>% slice(0:timesteps)
  
  RC <- seq(0, timesteps, 1)
  RC <- (RCsec * RC * Dsec) /(V0 + (RC * Dsec)) #Vc, volume of secs at each timestep
  
  #input actual conductivity at each timestep
  EC <- c(EC0,k_measurements$cond_k)
  
  k_cal <- tibble(EC, RC)
  plot6 <- ggplot(k_cal, aes(x = EC, y = RC))+
    geom_smooth(method = 'lm', se = FALSE, color = "grey")+
    geom_point()+
    theme_classic()
  
  #calculate k
  #slope of the line between cond and relative cond from calibration tank
  k <- unname(lm(RC~EC)$coefficients[2])
  #back to calculating Q
  DeltaT <- meta$timestep #[s]
  
  #use upstream and downstream cond measurements
  cond <- read_csv(paste0(path,"/cond.csv")) %>% 
    mutate(time_down = mdy_hm(time_down), 
           time_up = mdy_hm(time_up)) 
  
  cond_summary <- cond %>% 
    summarise(min = min(cond_down),
              max = max(cond_down))
  
  #View(cond_summary)
    
    plot6 + geom_vline(data = cond_summary, aes(xintercept = min))+
    geom_vline(data = cond_summary, aes(xintercept = max))
  #return()
}
plotK("./salt_dilutions/FB/FB_8_1")
plotK("./salt_dilutions/FB/FB_6_23_fail")
plotK("./salt_dilutions/ZZ/ZZ_8_1")
plotK("./salt_dilutions/ZZ/ZZ_7_1")
plotK("./salt_dilutions/ZZ/ZZ_6_23") #different timestep, after fixing gave negative number
plotK("./salt_dilutions/ZZ/ZZ_6_21")

#maybe also write a function to preview the RC calculations, determine how many to omit
calcQ("./salt_dilutions/FB/FB_8_1") %>% mutate("shed" = "FB")
calcQ("./salt_dilutions/FB/FB_6_23_fail")
calcQ("./salt_dilutions/ZZ/ZZ_8_1")
calcQ("./salt_dilutions/ZZ/ZZ_7_1")
calcQ("./salt_dilutions/ZZ/ZZ_6_23") #different timestep, after fixing gave negative number
calcQ("./salt_dilutions/ZZ/ZZ_6_21")

mean_k <- mean(c(0.0001120716, 2.621858e-06, #FB
               9.453437e-05, 4.017994e-06,
               3.962093e-05, 9.693616e-05)) #ZZ
#no k
calcQ_noK("./salt_dilutions/FB/FB_7_1", mean(c(0.0001120716, 2.621858e-06)))
#still debating k value
calcQ_noK("./salt_dilutions/FB/FB_6_23_Success", mean(c(0.0001120716, 2.621858e-06)))
calcQ_noK("./salt_dilutions/FB/FB_6_23_fail", mean_k)

#missing FB 8_5, no k and no upstream logger
# ZZ_8_5, no k and no upstream logger
calcQ_noK_oneL("./salt_dilutions/FB/FB_8_5", 2.5e-04)
calcQ_noK_oneL("./salt_dilutions/ZZ/ZZ_8_5", mean(c(9.453437e-05, 4.017994e-06,
                                                    3.962093e-05, 9.693616e-05)))

calcQ_noK("./salt_dilutions/FB/FB_8_1", mean_k)
calcQ("./salt_dilutions/FB/FB_8_1")
#all combined
q_combined <- rbind(
calcQ("./salt_dilutions/FB/FB_8_1") %>% mutate("shed" = "FB"),
#calcQ("./salt_dilutions/FB/FB_6_23_fail") %>% mutate("shed" = "FB"),
calcQ("./salt_dilutions/ZZ/ZZ_8_1")%>% mutate("shed" = "ZZ"),
calcQ("./salt_dilutions/ZZ/ZZ_7_1")%>% mutate("shed" = "ZZ"),
#calcQ("./salt_dilutions/ZZ/ZZ_6_23") %>% mutate("shed" = "ZZ"),#different timestep, after fixing gave negative number
calcQ("./salt_dilutions/ZZ/ZZ_6_21")%>% mutate("shed" = "ZZ"),

#no k
calcQ_noK("./salt_dilutions/FB/FB_7_1", mean(c(0.0001120716, 2.621858e-06))) %>% mutate("shed" = "FB"),
calcQ_noK("./salt_dilutions/FB/FB_6_23_Success", mean(c(0.0001120716))) %>% mutate("shed" = "FB"),

#missing FB 8_5, no k and no upstream logger
# ZZ_8_5, no k and no upstream logger
calcQ_noK_oneL("./salt_dilutions/FB/FB_8_5", mean(c(0.0001120716, 2.621858e-06))) %>% mutate("shed" = "FB"),
calcQ_noK_oneL("./salt_dilutions/ZZ/ZZ_8_5", mean(c(9.453437e-05, 4.017994e-06,
                                                    3.962093e-05, 9.693616e-05)))%>% mutate("shed" = "ZZ")
)
