
# Early Cost Effectiveness Model in Rectal Cancer ===
# This report contains A summary of economics value of dostarlimab 
# in neo-adjuvant  settings in patients with newly diagnosed rectal cancer 
# vs standard of care pathway. 


# 1 - Call functions  ======

rm(list = ls(all = TRUE))
options(warn=-1)
zero <- 0

# ++++++ The structure of this R-script +++++++++++++++++
# Load libraries 
# Read model input data from an external file
# Assign valued to the variables
# Calculate dependencies 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Define the functions
source("functions/foo.R")

#  Load the necessary libraries, some may need to be installed 
libraries <- load_libraries()
parameters <- read_model_params()

# Clean previously generated charts from the output directory 
a<-clean_output_dir(file_list = files_to_remove)

# (temp) Model parameters in the *csv files =========


# 1  # d_r_cost                0.035    annual discount rate, costs
# 2  # d_r_outc                0.035    annual discount rate, outcomes
# 3  # horizon                25.000    time horizon
# 4  # model_cycle            21.000    fixed parameter
# 5  # days_in_year          365.000    fixed parameter
# 6  # wtp                 30000.000    UK willingness to pay (ICER threshold)
# 7  # n_age_init              60       starting age in the cohort
# 8  # prop_males              0.600    proportion of males in the cohort

# 9  # smr_dfs_all             1.200    assumption, to be calibrated
# 10 # smr_dostar              1.000    assumption, to be calibrated
# 11 # smr_dfs                 1.100    assumption, to be calibrated
# 12 # smr_dfs1                1.100    assumption, to be calibrated
# 13 # smr_dfs2                1.100    assumption, to be calibrated     
# 14 # smr_rd                  1.500    assumption, to be calibrated     

# 15 # u_dfs_all               0.850    health state utility values     
# 16 # u_rm_all                0.650    health state utility values     
# 17 # u_perfect               1.000    health state utility values     
# 18 # u_Dostar                0.850    health state utility values     
# 19 # u_chemoRT               0.700    health state utility values     
# 20 # u_DFS                   0.850    health state utility values     
# 21 # u_DFS1                  0.850    health state utility values     
# 22 # u_Surg1                 0.600    health state utility values     
# 23 # u_Surg2                 0.600    health state utility values     
# 24 # u_DFS2                  0.850    health state utility values     
# 25 # u_RD                    0.700    health state utility values     
# 26 # u_RM                    0.650    health state utility values     
# 27 # u_RM1                   0.650    health state utility values     
# 28 # u_RM2                   0.650    health state utility values     

# 29 # c_dostar_per_cycle   5887.330    cost input
# 30 # n_dostar_cycles         9.000    cost input
# 31 # c_dfs_per_cycle        50.000    cost input
# 32 # c_dfs1_per_cycle       50.000    cost input
# 33 # c_dfs2_per_cycle       50.000    cost input
# 34 # c_chemoRT_per_cycle 10000.000    cost input
# 35 # c_surgery           10000.000    cost input
# 36 # c_rd_per_cycle       1000.000    cost input
# 37 # c_rmd_per_cycle      1000.000    cost input
# 38 # c_rmd1_per_cycle     1500.000    cost input
# 39 # c_rmd2_per_cycle     2000.000    cost input

# 40 # p_death_rm_all	            0.01426
# 41 # p_dfs_rm_all	              0.002
# 42 # p_d_dostar	                0
# 43 # p_dostar_discont_chemort	  0
# 44 # p_dostar_efficacy	        0.95
# 45 # p_dfs_rm	                  0.002
# 46 # p_dfs_chemort	            0
# 47 # p_d_dfs	                  0
# 48 # p_d_rm	                    0.01426
# 49 # p_d_chemort	              0.01
# 50 # prop_chemort_dfs	          0.25
# 51 # ELG	                      0.95
# 52 # prop_eligibility_surgery	  0.95
# 53 # prop_surg1_dfs2	          0.75
# 54 # prop_surg2_dfs2 	          0.75
# 55 # prop_chemort_surgery 	    1
# 56 # p_d_perioperative	       0.006
# 57 # prop_chemort_dfs2	       0.4
# 58 # p_d_DFS1	                 0.002
# 59 # prop_dfs_to_surgery	     0.5
# 60 # p_d_DFS2	                 0.001
# 61 # p_d_rd	                   0.045
# 62 # p_rd_rm2	                 0.05




# basic information model:
Strategies     <- c("Standard care", "Dostarlimab") # strategy names

# 2 - Read model parameters from an external file =====

# Read params from a *.csv file
df_params   <- read_model_params()
# Assign values to model params
action      <- load_base_case_inputs()
# Calculate dependencies 
action      <- calc_variables()


# Create tables/data frames for the automated reports

kbl_inputs_mortality    <- create_table_report_smr()
model_inputs_utilities  <- create_table_report_utilities()
model_inputs_costs      <- create_table_report_costs()
probs_table             <- create_table_report_probs()
kbl_inputs_probs        <- create_table_report_probs()

plot_and_save_life_tables()

# Create a column with general population mortality 
LT_temp          <-  subset(LT,age>= n_age_init)
LT_temp$gen_pop  <- (LT_temp$males*(prop_males) +  LT_temp$females*(1-prop_males))/2 
LT_temp$gen_pop  <-  LT_temp$gen_pop/LT_temp$gen_pop[1]
LT_temp          <-  LT_temp$gen_pop
prop_eligibility_surgery = ELG


# ----- Run base case --------------------------------------

m_TR_soc <- eem(1, 
                        prop_start_soc,
                        prop_males,
                        n_age_init, 
                        smr_dfs_all,
                        smr_dostar,
                        smr_dfs,
                        smr_dfs1,
                        smr_dfs2,
                        smr_rd,
                        u_Dostar,     
                        u_chemoRT,     
                        u_DFS,
                        u_DFS1,
                        u_Surg1,   
                        u_Surg2,
                        u_DFS2,
                        u_RD,     
                        u_RM,
                        u_RM1,
                        u_RM2,
                        prop_chemort_dfs1,
                        prop_eligibility_surgery,
                        prop_surg1_dfs2,
                        prop_surg2_dfs2,
                        zero,
                        n_dostar_cycles, 
                        c_dfs_per_cycle,
                        c_dfs1_per_cycle,
                        c_dfs2_per_cycle,
                        c_chemoRT_per_cycle,
                        c_surgery,
                        c_rd_per_cycle,
                        c_rmd_per_cycle,
                        c_rmd1_per_cycle,
                        c_rmd2_per_cycle
                        ) 


QALY_soc <- m_TR_soc[[3]]
cost_soc <- m_TR_soc[[4]]


m_TR_dostar  <- eem(2, 
                            prop_start_dostar,
                            prop_males, 
                            n_age_init, 
                  
                            smr_dfs_all,
                            smr_dostar,
                            smr_dfs,
                            smr_dfs1,
                            smr_dfs2,
                            smr_rd,   
                    
                            u_Dostar,      
                            u_chemoRT,     
                            u_DFS,    
                            u_DFS1,      
                            u_Surg1,     
                            u_Surg2,     
                            u_DFS2,      
                            u_RD,       
                            u_RM,      
                            u_RM1,       
                            u_RM2,
                            
                    
                            prop_chemort_dfs1,
                            prop_eligibility_surgery,
                            prop_surg1_dfs2,
                            prop_surg2_dfs2,
                    
                            c_dostar_per_cycle,
                            n_dostar_cycles, 
                            c_dfs_per_cycle,
                            c_dfs1_per_cycle,
                            c_dfs2_per_cycle,
                            c_chemoRT_per_cycle,
                            c_surgery,
                            c_rd_per_cycle,
                            c_rmd_per_cycle,
                            c_rmd1_per_cycle,
                            c_rmd2_per_cycle
                            )

QALY_dostar <- m_TR_dostar[[3]]
cost_dostar <- m_TR_dostar[[4]]

ICER_base_case <- (cost_dostar-cost_soc)/(QALY_dostar-QALY_soc)


# ----- Run base case --------------------------------------



plot_markov_traces()


# Time in disease states bar plot ----

m_TR_dostar_barplot <- subset(m_TR_dostar_short, select = c(Dostar, DFS,  RM, ChemoRT, DFS1, RM1, Surg1, Surg2, RD, DFS2, RM2, OS))
m_TR_soc_barplot <- subset(m_TR_soc_short, select = c(Dostar, DFS,  RM, ChemoRT, DFS1, RM1, Surg1, Surg2, RD, DFS2, RM2, OS))

m_TR_dostar_sums <- colSums(m_TR_dostar_barplot)
m_TR_soc_sums <- colSums(m_TR_soc_barplot)

state <- c("01.Dost", "02.DFS",  "03.RM", "04.CRT", "05.DFS1", "06.RM1", "07.Sur1", "08.Sur2", "09.RD", "10.DFS2", "11.RM2", "12.OS", 
           "01.Dost", "02.DFS",  "03.RM", "04.CRT", "05.DFS1", "06.RM1", "07.Sur1", "08.Sur2", "09.RD", "10.DFS2", "11.RM2", "12.OS")

arm <- c("Dostar", "Dostar",  "Dostar", "Dostar", "Dostar", "Dostar", "Dostar", "Dostar", "Dostar", "Dostar", "Dostar", "Dostar", 
         "SoC", "SoC",  "SoC", "SoC", "SoC", "SoC", "SoC", "SoC", "SoC", "SoC", "SoC", "SoC")

time <- c(m_TR_dostar_sums[1]*(3/52), m_TR_dostar_sums[2]*(3/52), m_TR_dostar_sums[3]*(3/52), 
          m_TR_dostar_sums[4]*(3/52), m_TR_dostar_sums[5]*(3/52), m_TR_dostar_sums[6]*(3/52),
          m_TR_dostar_sums[7]*(3/52), m_TR_dostar_sums[8]*(3/52), m_TR_dostar_sums[9]*(3/52),
          m_TR_dostar_sums[10]*(3/52), m_TR_dostar_sums[11]*(3/52), m_TR_dostar_sums[12]*(3/52),
          
          m_TR_soc_sums[1]*(3/52), m_TR_soc_sums[2]*(3/52), m_TR_soc_sums[3]*(3/52), 
          m_TR_soc_sums[4]*(3/52), m_TR_soc_sums[5]*(3/52), m_TR_soc_sums[6]*(3/52),
          m_TR_soc_sums[7]*(3/52), m_TR_soc_sums[8]*(3/52), m_TR_soc_sums[9]*(3/52),
          m_TR_soc_sums[10]*(3/52), m_TR_soc_sums[11]*(3/52), m_TR_soc_sums[12]*(3/52))

df_barplot <-data.frame(state, arm, time)
df_barplot$time <- round(df_barplot$time,2) 

time_in_state_barplot <- ggplot(df_barplot, aes(x=state, y=time, fill=arm)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_viridis(discrete=TRUE, name="") +
  scale_color_viridis(discrete = TRUE)+
  # geom_text(aes(label=time),  hjust = 1, nudge_x = -.5)+
  theme_bw() +
  xlab("Disease state")+
  ylab("Time in state")


ggsave(time_in_state_barplot, filename = "report/images/time_in_state.png", 
       width = 20, height = 12, units = "cm")


### Plot survival curves in the model arms
## The base case traces are 

# m_TR_dostar_short
# m_TR_soc_short

#  select time and OS from the SoC data frame 

df_soc <- m_TR_soc_short %>% select(tm, OS) 
df_dostar <- m_TR_dostar_short %>% select(OS, gen_pop)

# naming columns 

colnames(df_soc) <- c("years", "OS SoC")
colnames(df_dostar) <- c("OS Dostar", "Life_tables")
df <-data.frame(df_soc, df_dostar)


# reshape the data 

m_OS_plot <- df %>% 
  tidyr::pivot_longer(cols = 
                        c("OS.SoC", 
                          "OS.Dostar", 
                          "Life_tables",
                        ), names_to ="Survival",values_to="probability")


plot_OS <- ggplot(data = m_OS_plot,
                  aes(x = years, y = probability, col = Survival))+
  theme_base()+
  xlab("Time, years")+
  ylab("Proporttion alive")+
  geom_line(linewidth = 1)

# plot_OS 

ggsave(plot = plot_OS, filename = "report/images/plot_OS.png", 
       width = 20, height = 12, units = "cm")


## Demo results

# Generate outputs in 'natural units"
# Total costs 

# Run the model for SoC 

cost_total_soc <- sum(m_TR_subset_input_soc[,'cost_total'])
cost_total_dostar <- sum(m_TR_subset_input_dostar[,'cost_total'])
cost_total_incr <- cost_total_dostar - cost_total_soc

# Total QALYs
QALY_total_soc <- sum(m_TR_subset_input_soc[,'QALY_total'])*(3/52)
QALY_total_dostar <- sum(m_TR_subset_input_dostar[,'QALY_total'])*(3/52)
QALY_total_incr <-QALY_total_dostar - QALY_total_soc

# 
QALY_breakdown_dostar_soc <- 
  sum(m_TR_subset_input_soc[,'QALY_Dostar'])*(3/52)
QALY_breakdown_dostar_dostar <- 
  sum(m_TR_subset_input_dostar[,'QALY_Dostar'])*(3/52)


tbl_QALY_breakdown_dostar_soc <- 
  paste(as.character(round(as.numeric(QALY_breakdown_dostar_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_dostar_soc/QALY_total_soc),1)), 
        "%)", sep = "")

# Shows the QALY breakdown also as a % of the total QALY

tbl_QALY_breakdown_dostar_dostar <-
  paste(as.character(round(as.numeric(QALY_breakdown_dostar_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_dostar_dostar/QALY_total_dostar), 
                                  1)),"%)", sep = "")

tbl_QALY_breakdown_dostar_incr <-
  as.character(round(as.numeric(QALY_breakdown_dostar_dostar-
                                  QALY_breakdown_dostar_soc),3))

# Values calculated from the model 
QALY_breakdown_dfs_soc <- 
  sum(m_TR_subset_input_soc[,'QALY_DFS'])*(3/52)

QALY_breakdown_dfs_dostar <-  
  sum(m_TR_subset_input_dostar[,'QALY_DFS'])*(3/52)


# Tech lines for presentaton in the table
tbl_QALY_breakdown_dfs_soc <- 
  paste(as.character(round(as.numeric(QALY_breakdown_dfs_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_dfs_soc/QALY_total_soc),1)),
        "%)", sep = "")  

tbl_QALY_breakdown_dfs_dostar <-
  paste(as.character(round(as.numeric(QALY_breakdown_dfs_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_dfs_dostar/QALY_total_dostar), 
                                  1)),"%)", sep = "")   

tbl_QALY_breakdown_dfs_incr <-
  as.character(round(as.numeric(QALY_breakdown_dfs_dostar -
                                  QALY_breakdown_dfs_soc),3))

QALY_breakdown_rm_soc <- 
  sum(m_TR_subset_input_soc[,'QALY_RM'])*(3/52)

QALY_breakdown_rm_dostar <- 
  sum(m_TR_subset_input_dostar[,'QALY_RM'])*(3/52)

QALY_breakdown_rm_incr <- 
  QALY_breakdown_rm_dostar-QALY_breakdown_rm_soc


tbl_QALY_breakdown_rm_soc <- 
  paste(as.character(round(as.numeric(QALY_breakdown_rm_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_rm_soc/QALY_total_soc),1)),"%)", 
        sep = "")


tbl_QALY_breakdown_rm_dostar <- 
  paste(as.character(round(as.numeric(QALY_breakdown_rm_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_rm_dostar/QALY_total_dostar),1)),"%)", sep = "")


tbl_QALY_breakdown_rm_incr <- 
  as.character(round(as.numeric(QALY_breakdown_rm_incr),3))


QALY_breakdown_dfs1_soc <- 
  sum(m_TR_subset_input_soc[,'QALY_DFS1'])*(3/52)

QALY_breakdown_dfs1_dostar <- 
  sum(m_TR_subset_input_dostar[,'QALY_DFS1'])*(3/52)


tbl_QALY_breakdown_dfs1_soc <- 
  paste(as.character(round(as.numeric(QALY_breakdown_dfs1_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_dfs1_soc/QALY_total_soc),1)),"%)", 
        sep = "")           


tbl_QALY_breakdown_dfs1_dostar <-
  paste(as.character(round(as.numeric(QALY_breakdown_dfs1_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_dfs1_dostar/QALY_total_dostar),1)),"%)",  sep = "")   


QALY_breakdown_surgery_soc <- 
  sum(m_TR_subset_input_soc[,'QALY_Surg1'])*(3/52)+
  sum(m_TR_subset_input_soc[,'QALY_Surg2'])*(3/52)

QALY_breakdown_surgery_dostar <- 
  sum(m_TR_subset_input_dostar[,'QALY_Surg1'])*(3/52)+
  sum(m_TR_subset_input_dostar[,'QALY_Surg2'])*(3/52)

QALY_breakdown_surgery_incr <- QALY_breakdown_surgery_dostar - QALY_breakdown_surgery_soc

tbl_QALY_breakdown_surgery_soc    <- as.character(round(as.numeric(QALY_breakdown_surgery_soc),3))
tbl_QALY_breakdown_surgery_dostar <- as.character(round(as.numeric(QALY_breakdown_surgery_dostar),3))
tbl_QALY_breakdown_surgery_incr   <- as.character(round(as.numeric(QALY_breakdown_surgery_incr),3))

# Incrementals            

tbl_QALY_breakdown_dfs1_incr <-
  as.character(round(as.numeric(QALY_breakdown_dfs1_dostar - QALY_breakdown_dfs1_soc),3))

QALY_breakdown_dfs2_soc    <- sum(m_TR_subset_input_soc[,'QALY_DFS2'])*(3/52)
QALY_breakdown_dfs2_dostar <- sum(m_TR_subset_input_dostar[,'QALY_DFS2'])*(3/52)


tbl_QALY_breakdown_dfs2_soc <- paste(as.character(round(as.numeric(QALY_breakdown_dfs2_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_dfs2_soc/QALY_total_soc), 1)),"%)",  sep = "")            

tbl_QALY_breakdown_dfs2_dostar <- paste(as.character(round(as.numeric(QALY_breakdown_dfs2_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_dfs2_dostar/QALY_total_dostar), 1)) ,"%)",  sep = "") 

tbl_QALY_breakdown_dfs2_incr <- as.character(round(as.numeric(QALY_breakdown_dfs2_dostar - QALY_breakdown_dfs2_soc),3))


QALY_breakdown_rm1_soc <- sum(m_TR_subset_input_soc[,'QALY_RM1'])*(3/52)

QALY_breakdown_rm1_dostar <- sum(m_TR_subset_input_dostar[,'QALY_RM1'])*(3/52)

QALY_breakdown_rm1_incr <-  QALY_breakdown_rm1_dostar-QALY_breakdown_rm1_soc


tbl_QALY_breakdown_rm1_soc <-  paste(as.character(round(as.numeric(QALY_breakdown_rm1_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_rm1_soc/QALY_total_soc),1)),"%)",  sep = "")          

tbl_QALY_breakdown_rm1_dostar <- paste(as.character(round(as.numeric(QALY_breakdown_rm1_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_rm1_dostar/QALY_total_dostar), 1)),"%)",  sep = "")          

tbl_QALY_breakdown_rm1_incr <- as.character(round(as.numeric(QALY_breakdown_rm1_incr),3))


QALY_breakdown_rd_soc <- sum(m_TR_subset_input_soc[,'QALY_RD'])*(3/52)
QALY_breakdown_rd_dostar <- sum(m_TR_subset_input_dostar[,'QALY_RD'])*(3/52)

tbl_QALY_breakdown_rd_soc <- paste(as.character(round(as.numeric(QALY_breakdown_rd_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_rd_soc/QALY_total_soc),1)),"%)",  sep = "")     

tbl_QALY_breakdown_rd_dostar <- paste(as.character(round(as.numeric(QALY_breakdown_rd_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_rd_dostar/QALY_total_dostar), 1)),"%)",  sep = "")          

tbl_QALY_breakdown_rd_incr <- as.character(round(as.numeric(QALY_breakdown_rd_dostar - QALY_breakdown_rd_soc),3))
QALY_breakdown_chemoRT_soc <- sum(m_TR_subset_input_soc[,'QALY_chemoRT'])*(3/52)

QALY_breakdown_chemoRT_dostar <- sum(m_TR_subset_input_dostar[,'QALY_chemoRT'])*(3/52)


tbl_QALY_breakdown_chemoRT_soc <- paste(as.character(round(as.numeric(QALY_breakdown_chemoRT_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_chemoRT_soc/QALY_total_soc), 1)),"%)",  sep = "")            

tbl_QALY_breakdown_chemoRT_dostar <-  paste(as.character(round(as.numeric(QALY_breakdown_chemoRT_dostar),3)),
        " (", as.character(round(100*(QALY_breakdown_chemoRT_dostar/QALY_total_dostar), 1)),"%)",  sep = "")  

tbl_QALY_breakdown_chemoRT_incr <- as.character(round(as.numeric(QALY_breakdown_chemoRT_dostar - QALY_breakdown_chemoRT_soc),3))


QALY_breakdown_rm2_soc <- sum(m_TR_subset_input_soc[,'QALY_RM2'])*(3/52)
QALY_breakdown_rm2_dostar <- sum(m_TR_subset_input_dostar[,'QALY_RM2'])*(3/52)
QALY_breakdown_rm2_incr <- QALY_breakdown_rm2_dostar-QALY_breakdown_rm2_soc

tbl_QALY_breakdown_rm2_soc <- paste(as.character(round(as.numeric(QALY_breakdown_rm2_soc),3)),
        " (", as.character( round(100*(QALY_breakdown_rm2_soc/QALY_total_soc), 1)),"%)",  sep = "")           
tbl_QALY_breakdown_rm2_dostar <- paste(as.character(round(as.numeric(QALY_breakdown_rm2_dostar),3)),
        " (", as.character( round(100*(QALY_breakdown_rm2_dostar/QALY_total_dostar), 1)) ,"%)",  sep = "")   
tbl_QALY_breakdown_rm2_incr <- as.character(round(as.numeric(QALY_breakdown_rm2_incr),3))



ICER  <- cost_total_incr/QALY_total_incr 
ICER_str<- paste("£", as.character(format(round(as.numeric(ICER), 0), nsmall=0, big.mark=","))) 

# Cost breakdown 


cost_breakdown_dostar_soc     <- sum(m_TR_subset_input_soc[,'c_dostar'])
cost_breakdown_dostar_dostar  <- sum(m_TR_subset_input_dostar[,'c_dostar'])

#field c_dfs
cost_breakdown_dfs_soc        <- sum(m_TR_subset_input_soc[,'c_dfs'])
cost_breakdown_dfs_dostar     <- sum(m_TR_subset_input_dostar[,'c_dfs'])


#field c_rmd
cost_breakdown_rmd_soc        <- sum(m_TR_subset_input_soc[,'c_rmd'])
cost_breakdown_rmd_dostar     <- sum(m_TR_subset_input_dostar[,'c_rmd'])

#field c_chemoRT
cost_breakdown_chemoRT_soc    <- sum(m_TR_subset_input_soc[,'c_chemoRT'])
cost_breakdown_chemoRT_dostar <- sum(m_TR_subset_input_dostar[,'c_chemoRT'])

cost_breakdown_chemoRT_incr   <- cost_breakdown_chemoRT_dostar - cost_breakdown_chemoRT_soc

#field c_dfs1
cost_breakdown_dfs1_soc       <- sum(m_TR_subset_input_soc[,'c_dfs1'])
cost_breakdown_dfs1_dostar    <- sum(m_TR_subset_input_dostar[,'c_dfs1'])

#field c_rmd1
cost_breakdown_rmd1_soc       <- sum(m_TR_subset_input_soc[,'c_rmd1'])
cost_breakdown_rmd1_dostar    <- sum(m_TR_subset_input_dostar[,'c_rmd1'])

#field c_dfs2
cost_breakdown_dfs2_soc       <- sum(m_TR_subset_input_soc[,'c_dfs2'])
cost_breakdown_dfs2_dostar    <- sum(m_TR_subset_input_dostar[,'c_dfs2'])

#field c_rd
cost_breakdown_rd_soc         <- sum(m_TR_subset_input_soc[,'c_rd'])
cost_breakdown_rd_dostar      <- sum(m_TR_subset_input_dostar[,'c_rd'])

cost_breakdown_rd_incr        <- (cost_breakdown_rd_dostar - cost_breakdown_rd_soc) 

#field c_rmd2
cost_breakdown_rmd2_soc       <- sum(m_TR_subset_input_soc[,'c_rmd2'])
cost_breakdown_rmd2_dostar    <- sum(m_TR_subset_input_dostar[,'c_rmd2'])


# OALYs aggregated by main disease categories 

QALY_DFS_soc <- (sum(m_TR_subset_input_soc[,'QALY_Dostar'])+
                   sum(m_TR_subset_input_soc[,'QALY_DFS'])+
                   sum(m_TR_subset_input_soc[,'QALY_DFS1'])+
                   sum(m_TR_subset_input_soc[,'QALY_DFS2']))*(3/52)

QALY_DFS_dostar <- (sum(m_TR_subset_input_dostar[,'QALY_Dostar'])+
                      sum(m_TR_subset_input_dostar[,'QALY_DFS'])+
                      sum(m_TR_subset_input_dostar[,'QALY_DFS1'])+
                      sum(m_TR_subset_input_dostar[,'QALY_DFS2']))*(3/52)



# Prepare the output table - debug

row_headings <- c(
  "Cost, total",           # 1
  "Life years (LY)",       # 2
  "QALY",                  # 3
  "ICER, £ per qaly",      # 5
  "Alive - at year 1", 
  "Alive - at year 2",
  "Alive - at year 3",
  "Alive - at year 5",
  "Cost breakdown:",       #4
  "Dostarlimab",           #5
  "Disease-free (all)",
  "Chemoradiotherapy",
  "Surgery",
  "Residual disease (post-surgery)",
  "Recurrent/metastatic",
  "QALY breakdown:",
  "Dostarlimab",
  "DFS, post-dostar", 
  "Recurrent/metastatic, post-dostar",
  "Chemoradiotherapy",
  "DFS, post chemoRT",
  "Surgery (all)",
  "Recurrent/metastatic 1",
  "DFS, post-surgery",
  "Residual disease",
  "Recurrent/metastatic 2",
  "Key parameters",
  "Price Dostar, 500 mg vial",
  "Efficacy Dostarlimab")    


cost_soc<-paste("£", as.character( format(round(as.numeric(cost_total_soc), 0), nsmall=0, big.mark=",")))
cost_dostar<-paste("£", as.character( format(round(as.numeric(cost_total_dostar), 0),nsmall=0, big.mark=",")))
cost_incr<-paste("£", as.character( format(round(as.numeric(cost_total_incr), 0),nsmall=0, big.mark=",")))

# Cost breakdown 
# Dostarlimab 

tbl_cost_breakdown_dostar_dostar <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dostar_dostar), 0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_dostar_soc    <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dostar_soc),  0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_dostar_incr   <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dostar_dostar -  cost_breakdown_dostar_soc), 0), nsmall=0, big.mark=","))) 

# All disease-free survival states 

cost_breakdown_dfsall_dostar     <- cost_breakdown_dfs_dostar+cost_breakdown_dfs1_dostar+ cost_breakdown_dfs2_dostar
tbl_cost_breakdown_dfsall_dostar <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dfsall_dostar), 0), nsmall=0, big.mark=","))) 

cost_breakdown_dfsall_soc        <- cost_breakdown_dfs_soc+cost_breakdown_dfs1_soc+cost_breakdown_dfs2_soc
tbl_cost_breakdown_dfsall_soc    <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dfsall_soc), 0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_dfsall_dostar <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dfsall_dostar), 0), nsmall=0, big.mark=","))) 

cost_breakdown_dfsall_incr       <- cost_breakdown_dfsall_dostar - cost_breakdown_dfsall_soc
tbl_cost_breakdown_dfsall_incr   <- paste("£", as.character(format(round(as.numeric(cost_breakdown_dfsall_incr), 0), nsmall=0, big.mark=","))) 


# Chemo radiotherapy 
tbl_cost_breakdown_ChemoRT_dostar <-  paste("£", as.character(format(round(as.numeric(cost_breakdown_chemoRT_dostar), 0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_ChemoRT_soc    <-  paste("£", as.character(format(round(as.numeric(cost_breakdown_chemoRT_soc), 0), nsmall=0, big.mark=",")))
tbl_cost_breakdown_chemoRT_incr   <-  paste("£", as.character(format(round(as.numeric(cost_breakdown_chemoRT_incr), 0), nsmall=0, big.mark=",")))


cost_breakdown_surgery_dostar     <- sum(m_TR_subset_input_dostar[,'c_Surg1'])+ sum(m_TR_subset_input_dostar[,'c_Surg2'])
cost_breakdown_surgery_soc        <- sum(m_TR_subset_input_soc[,'c_Surg1'])+sum(m_TR_subset_input_soc[,'c_Surg2'])

cost_breakdown_surgery_incr       <- (cost_breakdown_surgery_dostar-cost_breakdown_surgery_soc)


tbl_cost_breakdown_surgery_dostar <- paste("£", as.character(format(round(as.numeric(cost_breakdown_surgery_dostar), 0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_surgery_soc    <- paste("£", as.character(format(round(as.numeric(cost_breakdown_surgery_soc),  0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_surgery_incr   <- paste("£", as.character(format(round(as.numeric(cost_breakdown_surgery_incr), 0), nsmall=0, big.mark=","))) 


# Residual disease

tbl_cost_breakdown_rd_dostar  <-  paste("£", as.character(format(round(as.numeric(cost_breakdown_rd_dostar), 0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_rd_soc     <-  paste("£", as.character(format(round(as.numeric(cost_breakdown_rd_soc), 0), nsmall=0, big.mark=","))) 
tbl_cost_breakdown_rd_incr    <-  paste("£", as.character(format(round(as.numeric(cost_breakdown_rd_incr), 0), nsmall=0, big.mark=","))) 


# Recurrent metastatic - all 

cost_breakdown_all_rmd_dostar     <- cost_breakdown_rmd_dostar+cost_breakdown_rmd1_dostar+cost_breakdown_rmd2_dostar
cost_breakdown_all_rmd_soc        <- cost_breakdown_rmd_soc+cost_breakdown_rmd1_soc+cost_breakdown_rmd2_soc
cost_breakdown_all_rmd_incr       <- cost_breakdown_all_rmd_dostar-cost_breakdown_all_rmd_soc
tbl_cost_breakdown_all_rmd_dostar <- paste("£", as.character(format(round(as.numeric(cost_breakdown_all_rmd_dostar), 0), nsmall=0, big.mark=",")))
tbl_cost_breakdown_all_rmd_soc    <- paste("£", as.character(format(round(as.numeric(cost_breakdown_all_rmd_soc), 0), nsmall=0, big.mark=",")))
tbl_cost_breakdown_all_rmd_incr   <- paste("£", as.character( format(round(as.numeric(cost_breakdown_all_rmd_incr), 0), nsmall=0, big.mark=",")))


qaly_soc <- as.character(round(as.numeric(QALY_total_soc),2))
qaly_dostar <- as.character(round(as.numeric(QALY_total_dostar),2))
qaly_incr <- as.character(round(as.numeric(QALY_total_incr),2))

df_lm <- df


landmark_output <- landmark_os(dataset=df_lm, 1) # year 1
lm_surv_year_1_soc<- paste(as.character(round(landmark_output[2]*100,1), nsmall=1),"%", sep="")
lm_surv_year_1_dostar <- paste(as.character(round(landmark_output[3]*100,1), nsmall=1),"%", sep="")

lm_surv_year_1_incr <- paste(as.character(round((landmark_output[3] - landmark_output[2])*100,1), nsmall=1),"%", sep="")

# paste(as.character(round(markov_trace[temp_row,colnum]*100,1), 
# nsmall=1),"%", sep="")

landmark_output <- landmark_os(df_lm, 2) # year 2
lm_surv_year_2_soc    <- paste(as.character(round(landmark_output[2]*100,1), nsmall=1),"%", sep="")
lm_surv_year_2_dostar <- paste(as.character(round(landmark_output[3]*100,1), nsmall=1),"%", sep="")
lm_surv_year_2_incr <- paste(as.character(round((landmark_output[3] - landmark_output[2])*100,1), nsmall=1),"%", sep="")

landmark_output <- landmark_os(df_lm, 3)  # year 3
lm_surv_year_3_soc    <- paste(as.character(round(landmark_output[2]*100,1), nsmall=1),"%", sep="")
lm_surv_year_3_dostar <- paste(as.character(round(landmark_output[3]*100,1), nsmall=1),"%", sep="")
lm_surv_year_3_incr <- paste(as.character(round((landmark_output[3] - landmark_output[2])*100,1), nsmall=1),"%", sep="")


landmark_output <- landmark_os(df_lm, 5)  # year 5
lm_surv_year_5_soc    <- paste(as.character(round(landmark_output[2]*100,1), nsmall=1),"%", sep="")
lm_surv_year_5_dostar <- paste(as.character(round(landmark_output[3]*100,1), nsmall=1),"%", sep="")
lm_surv_year_5_incr <- paste(as.character(round((landmark_output[3] - landmark_output[2])*100,1), nsmall=1),"%", sep="")




soc <- c( 
  cost_soc,
  "-",
  qaly_soc,
  " ",
  lm_surv_year_1_soc,
  lm_surv_year_2_soc,
  lm_surv_year_3_soc,
  lm_surv_year_5_soc,
  " ",
  tbl_cost_breakdown_dostar_soc,
  tbl_cost_breakdown_dfsall_soc,
  tbl_cost_breakdown_ChemoRT_soc,
  tbl_cost_breakdown_surgery_soc,
  tbl_cost_breakdown_rd_soc,
  tbl_cost_breakdown_all_rmd_soc,
  " ",
  tbl_QALY_breakdown_dostar_soc,
  tbl_QALY_breakdown_dfs_soc,
  tbl_QALY_breakdown_rm_soc,
  tbl_QALY_breakdown_chemoRT_soc,
  tbl_QALY_breakdown_dfs1_soc,
  tbl_QALY_breakdown_surgery_soc,
  tbl_QALY_breakdown_rm1_soc,
  tbl_QALY_breakdown_dfs2_soc,
  tbl_QALY_breakdown_rd_soc,
  tbl_QALY_breakdown_rm2_soc,
  "",
  "",
  ""
)


dostar <- c(
  cost_dostar,
  "-",
  qaly_dostar,
  "-",
  lm_surv_year_1_dostar,
  lm_surv_year_2_dostar,
  lm_surv_year_3_dostar,
  lm_surv_year_5_dostar,
  "-",
  tbl_cost_breakdown_dostar_dostar,
  tbl_cost_breakdown_dfsall_dostar,
  tbl_cost_breakdown_ChemoRT_dostar,
  tbl_cost_breakdown_surgery_dostar,
  tbl_cost_breakdown_rd_dostar,
  tbl_cost_breakdown_all_rmd_dostar,
  " ",
  tbl_QALY_breakdown_dostar_dostar,
  tbl_QALY_breakdown_dfs_dostar,
  tbl_QALY_breakdown_rm_dostar,
  tbl_QALY_breakdown_chemoRT_dostar,
  tbl_QALY_breakdown_dfs1_dostar,
  tbl_QALY_breakdown_surgery_dostar,
  tbl_QALY_breakdown_rm1_dostar,
  tbl_QALY_breakdown_dfs2_dostar,
  tbl_QALY_breakdown_rd_dostar,
  tbl_QALY_breakdown_rm2_dostar,
  "",
  "",
  ""
)

incr <- c(
  cost_incr,
  "-",
  qaly_incr,
  ICER_str,
  "Y1",
  "Y2",
  "y3",
  "Y5",
  "-",
  tbl_cost_breakdown_dostar_incr,
  tbl_cost_breakdown_dfsall_incr,
  tbl_cost_breakdown_chemoRT_incr,
  tbl_cost_breakdown_surgery_incr,
  tbl_cost_breakdown_rd_incr,
  tbl_cost_breakdown_all_rmd_incr,
  " ",
  tbl_QALY_breakdown_dostar_incr,
  tbl_QALY_breakdown_dfs_incr,
  tbl_QALY_breakdown_rm_incr,
  tbl_QALY_breakdown_chemoRT_incr,
  tbl_QALY_breakdown_dfs1_incr,
  tbl_QALY_breakdown_surgery_incr,
  tbl_QALY_breakdown_rm1_incr,
  tbl_QALY_breakdown_dfs2_incr,
  tbl_QALY_breakdown_rd_incr,
  tbl_QALY_breakdown_rm2_incr,
  "",
  "",
  ""
)

model_output <- data.frame(Outcomes= row_headings,                  
                           SoC = soc,
                           Dostarlimab = dostar,
                           Incremental = incr)

save_markov_traces <- save_excel()


## 13.1 One-way sensitivity analysis 

###############################################################################


# This is a placeholder working example found in stackoverflow and 
# further converted into a working axample bt Rob Smith (DPA)
# we keep this example in the report as a placeholder to be further substituted 
# with the parameters and outcomes of the model 

# https://stackoverflow.com/questions/55751978/tornado-both-sided-horizontal-bar-plot-in-r-with-chart-axes-crosses-at-a-given
#  rm(list = ls())

# library(ggplot2)
# library(plyr)
# library(dplyr)
# library(tidyverse)

#' Find the order of the parameters based on the difference between upper and lower bounds
#'
#' This function takes a vector of parameter names, and corresponding upper and lower bounds, 
#' and returns the parameter names sorted in ascending order of the absolute difference 
#' between the upper and lower bounds.
#'
#' @param v_params A character vector of parameter names.
#' @param v_upper A numeric vector of the same length as v_params, representing the upper bounds for each parameter.
#' @param v_lower A numeric vector of the same length as v_params, representing the lower bounds for each parameter.
#' @return A character vector of the parameter names sorted in ascending order of the absolute difference between the upper and lower bounds.
#' @examples
#' params <- c("a", "b", "c")
#' upper <- c(10, 20, 30)

#' lower <- c(1, 5, 10)
#' find_parameter_order(v_params = params, v_upper = upper, v_lower = lower)
#' @export
#' @importFrom assertthat assert_that

find_parameter_order <- function(v_params,
                                 v_upper,
                                 v_lower){
  assertthat::assert_that(is.character(v_params))
  assertthat::assert_thatrt_that(is.numeric(v_upper))
  assertthat::assert_that(is.numeric(v_lower))
  
  abs_diff <- abs(v_upper - v_lower)
  assertthat::assert_that(all(abs_diff >=0), 
                          msg = "Diff not above or equal to 0")
  return(v_params[order(abs_diff)])
}


#'
#' @param v_params A character vector of parameter names.
#' @param v_upper A numeric vector of the same length as v_params, representing the upper bounds for each parameter.
#' @param v_lower A numeric vector of the same length as v_params, representing the lower bounds for each parameter.
#' @return A character vector of the parameter names sorted in ascending order of the absolute difference between the upper and lower bounds.
#' @examples
#' params <- c("a", "b", "c")
#' upper <- c(10, 20, 30)
#' lower <- c(1, 5, 10)
#' find_parameter_order(v_params = params, v_upper = upper, v_lower = lower)
#' @export
#' @importFrom assertthat assert_that

find_parameter_order <- function(v_params,
                                 v_upper,
                                 v_lower){
  assertthat::assert_that(is.character(v_params))
  assertthat::assert_that(is.numeric(v_upper))
  assertthat::assert_that(is.numeric(v_lower))
  
  abs_diff <- abs(v_upper - v_lower)
  assertthat::assert_that(all(abs_diff >=0), 
                          msg = "Diff not above or equal to 0")
  
  return(v_params[order(abs_diff)])
}

#' Build a data frame for plotting parameter bounds
#'
#' This function takes a data frame containing parameter names and their corresponding upper
#' and lower bounds, and a width for the plot. It returns a modified data frame suitable for
#' use with ggplot2's geom_rect function.
#'
#' @param df A data frame containing columns 'Parameter' (character), 'Upper_Bound' (numeric), and 'Lower_Bound' (numeric).
#' @param width A numeric value representing the width of the plot.
#' @return A modified data frame containing columns 'Parameter', 'UL_Difference', 'type', 'output.value', 'ymin', 'ymax', 'xmin', and 'xmax', suitable for use with ggplot2's geom_rect function.
#' @examples
#' df <- data.frame(Parameter = c("a", "b", "c"),
#'                  Upper_Bound = c(10, 20, 30),
#'                  Lower_Bound = c(1, 5, 10))
#' width <- 0.5
#' buildPlot_df(df, width)
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr gather

build_plot_df <- function(df,
                          width,
                          base_value){
  
  param_order <- find_parameter_order(v_params = df$Parameter, 
                                      v_upper = df$Upper_Bound, 
                                      v_lower = df$Lower_Bound)
  
  df |> 
    dplyr::mutate(UL_Difference = abs(Upper_Bound - Lower_Bound)) |>
    tidyr::gather(key = 'type',
                  value = 'output.value',
                  Lower_Bound:Upper_Bound)|>
    # create the columns for geom_rect
    dplyr::mutate(
      Parameter = factor(Parameter, levels = param_order),
      ymin = pmin(output.value, base_value),
      ymax = pmax(output.value, base_value),
      xmin = as.numeric(Parameter) - width / 2,
      xmax = as.numeric(Parameter) + width / 2
    )
  
}

#' Create a Tornado plot using ggplot2
#' This function takes a data frame containing parameter names and their corresponding upper
#' and lower bounds, a base value, and a width for the plot. It returns a ggplot2 object
#' representing a Tornado plot.
#'
#' @param df A data frame containing columns 'Parameter' (character), 'Upper_Bound' (numeric), and 'Lower_Bound' (numeric).
#' @param base_value A numeric value representing the base value (reference line) for the Tornado plot.
#' @param width A numeric value representing the width of the plot.
#' @return A ggplot2 object representing a Tornado plot.
#' @examples
#' df <- data.frame(Parameter = c("a", "b", "c"),
#'                  Upper_Bound = c(10, 20, 30),
#'                  Lower_Bound = c(1, 5, 10))
#' base_value <- 5
#' width <- 0.5
#' plot_gg_tornado(df, base_value, width)
#' @export
#' @importFrom ggplot2 ggplot aes geom_rect theme_bw theme geom_hline scale_x_continuous coord_flip
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_rect}}, \code{\link[ggplot2]{geom_hline}}, \code{\link[ggplot2]{coord_flip}}

plot_gg_tornado <- function(df,
                            base_value,
                            width) {
  # get the order of the parameters
  param_order <<- find_parameter_order(
    v_params = df$Parameter,
    v_upper = df$Upper_Bound,
    v_lower  = df$Lower_Bound
  )
  # convert the dataframe into plot format
  df_plot <- build_plot_df(df = df, 
                           width =  width,
                           base_value = base_value)
  
  ggplot(data = df_plot,
         aes(
           ymax = ymax,
           ymin = ymin,
           xmax = xmax,
           xmin = xmin,
           fill = type
         )) +
    geom_rect() +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      legend.position = 'bottom',
      legend.title = element_blank()
    ) +
    geom_hline(yintercept = base_value) +
    scale_x_continuous(breaks = c(1:length(param_order)),
                       labels = param_order) +
    coord_flip()
}

# WORKED EXAMPLE #

# Read in example dataframe with direction one way only.
# df_example <- data.frame(Parameter = paste0("Parameter_0",1:9),
#                         Lower_Bound = runif(n = 9, min = 0, max = 10000),
#                         Upper_Bound = runif(n = 9, min = 10000, max = 20000))

# flip odds - so direction of effect is inverted for odd rows
# df_example[c(1,3,5,7,9),c("Lower_Bound", "Upper_Bound")] <- df_example[c(1,3,5,7,9),c("Upper_Bound", "Lower_Bound")]

# Create plot
# #plot_gg_tornado(df = df_example,
#                 base_value = 9504,
#                width = 0.95)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++ Dummy for OWSA with EEM ++++++++++++++++++++++++++++++++++++++++++++++

df_params_tornado <- df_params %>%
  select(var, Label, value, lower, upper, dsa) %>%
  filter(dsa=="yes") %>%
  select(var, Label, value, lower, upper)

df_params_tornado$Upper_Bound <- -1
df_params_tornado$Lower_Bound <- -2


for (row in 1:nrow(df_params_tornado))
{
  
  # Calculate model for lower range of the parameter 
  message(paste0(df_params_tornado[row,"var"], "<<-", df_params_tornado[row,"lower"]))
  run <<- paste0(df_params_tornado[row,"var"], "<<-", df_params_tornado[row,"lower"])
  message(run)
  eval(parse(text=run))
  
  
  # Calculate dependencies 
  action      <- calc_variables()
  
  

  m_TR_tornado_soc <- eem(1, 
                         prop_start_soc,
                         prop_males,
                         n_age_init, 
                         
                         smr_dfs_all,
                         smr_dostar,
                         smr_dfs,
                         smr_dfs1,
                         smr_dfs2,
                         smr_rd,              
                         
                         u_Dostar,      
                         u_chemoRT,     
                         u_DFS,    
                         u_DFS1,      
                         u_Surg1,     
                         u_Surg2,     
                         u_DFS2,      
                         u_RD,       
                         u_RM,      
                         u_RM1,       
                         u_RM2,
                         
                         prop_chemort_dfs1,
                         prop_eligibility_surgery,
                         prop_surg1_dfs2,
                         prop_surg2_dfs2,
                          
                         zero,
                         n_dostar_cycles, 
                         c_dfs_per_cycle,
                         c_dfs1_per_cycle,
                         c_dfs2_per_cycle,
                         c_chemoRT_per_cycle,
                         c_surgery,
                         c_rd_per_cycle,
                         c_rmd_per_cycle,
                         c_rmd1_per_cycle,
                         c_rmd2_per_cycle
                         ) 
  
  QALY_tornado_soc <- m_TR_tornado_soc[[3]]
  cost_tornado_soc <- m_TR_tornado_soc[[4]]
  
  m_TR_tornado_dostar  <- eem(2, 
                              prop_start_dostar,
                              prop_males,
                              n_age_init,
                              
                              smr_dfs_all,
                              smr_dostar,
                              smr_dfs,
                              smr_dfs1,
                              smr_dfs2,
                              smr_rd, 
                              
                              u_Dostar,      
                              u_chemoRT,     
                              u_DFS,    
                              u_DFS1,      
                              u_Surg1,     
                              u_Surg2,     
                              u_DFS2,      
                              u_RD,       
                              u_RM,      
                              u_RM1,       
                              u_RM2,
                              
                              prop_chemort_dfs1,
                              prop_eligibility_surgery,
                              prop_surg1_dfs2,
                              prop_surg2_dfs2,
                              
                              c_dostar_per_cycle,
                              n_dostar_cycles, 
                              c_dfs_per_cycle,
                              c_dfs1_per_cycle,
                              c_dfs2_per_cycle,
                              c_chemoRT_per_cycle,
                              c_surgery,
                              c_rd_per_cycle,
                              c_rmd_per_cycle,
                              c_rmd1_per_cycle,
                              c_rmd2_per_cycle
                              )
  
  QALY_tornado_dostar <- m_TR_tornado_dostar[[3]]
   cost_tornado_dostar <- m_TR_tornado_dostar[[4]]
  icer_tornado <- round((cost_tornado_dostar - cost_tornado_soc)/(QALY_tornado_dostar-QALY_tornado_soc),0)
  message(paste0("icer = "), icer_tornado)
  df_params_tornado[row,"Lower_Bound"] <- icer_tornado 
  
  
  
  # Calculate model for the upper  range of the parameter 
  message(paste0(df_params_tornado[row,"var"], "<<-", df_params_tornado[row,"upper"]))
  run <<- paste0(df_params_tornado[row,"var"], "<<-", df_params_tornado[row,"upper"])
  message(run)
  
  eval(parse(text=run))
  
        # Calculate dependencies 
      action      <- calc_variables()

 
  
   m_TR_tornado_soc <- eem(1, 
                          prop_start_soc,
                          prop_males,
                          n_age_init, 
                          
                          smr_dfs_all,
                          smr_dostar,
                          smr_dfs,
                          smr_dfs1,
                          smr_dfs2,
                          smr_rd, 
                          
                          u_Dostar,      
                          u_chemoRT,     
                          u_DFS,    
                          u_DFS1,      
                          u_Surg1,     
                          u_Surg2,     
                          u_DFS2,      
                          u_RD,       
                          u_RM,      
                          u_RM1,       
                          u_RM2,
                        
                          
                          prop_chemort_dfs1,
                          prop_eligibility_surgery,
                          prop_surg1_dfs2,
                          prop_surg2_dfs2,
                          
                          zero,
                          n_dostar_cycles, 
                          c_dfs_per_cycle,
                          c_dfs1_per_cycle,
                          c_dfs2_per_cycle,
                          c_chemoRT_per_cycle,
                          c_surgery,
                          c_rd_per_cycle,
                          c_rmd_per_cycle,
                          c_rmd1_per_cycle,
                          c_rmd2_per_cycle) 
  
   QALY_tornado_soc <- m_TR_tornado_soc[[3]]
   cost_tornado_soc <- m_TR_tornado_soc[[4]]
  
   m_TR_tornado_dostar <- eem(2, 
                          prop_start_dostar,
                          prop_males,
                          n_age_init, 
                               
                          smr_dfs_all,
                          smr_dostar,
                          smr_dfs,
                          smr_dfs1,
                          smr_dfs2,
                          smr_rd,              
                               
                          u_Dostar,      
                          u_chemoRT,     
                          u_DFS,    
                          u_DFS1,      
                          u_Surg1,     
                          u_Surg2,     
                          u_DFS2,      
                          u_RD,       
                          u_RM,      
                          u_RM1,       
                          u_RM2,
                               
                          prop_chemort_dfs1,
                          prop_eligibility_surgery,
                          prop_surg1_dfs2,
                          prop_surg2_dfs2,
                               
                          c_dostar_per_cycle,
                          n_dostar_cycles, 
                          c_dfs_per_cycle,
                          c_dfs1_per_cycle,
                          c_dfs2_per_cycle,
                          c_chemoRT_per_cycle,
                          c_surgery,
                          c_rd_per_cycle,
                          c_rmd_per_cycle,
                          c_rmd1_per_cycle,
                          c_rmd2_per_cycle
                          )
  
   QALY_tornado_dostar <- m_TR_tornado_dostar[[3]]
    cost_tornado_dostar <- m_TR_tornado_dostar[[4]]
    icer_tornado <- round((cost_tornado_dostar - cost_tornado_soc)/(QALY_tornado_dostar-QALY_tornado_soc),0)
    message(paste0("icer = "), icer_tornado)
    
    df_params_tornado[row,"Upper_Bound"] <- icer_tornado 

  
  # Return parameter to its base case value 
  message(paste0(df_params_tornado[row,"var"], "<<-", df_params_tornado[row,"value"]))
  run <<- paste0(df_params_tornado[row,"var"], "<<-", df_params_tornado[row,"value"])
  message(run)
  eval(parse(text=run))
  
  # Calculate dependencies 
  action      <- calc_variables()
  
}

df_params_tornado$Parameter <- df_params_tornado$Label
df_params_tornado <- df_params_tornado %>% 
  select(Parameter, Lower_Bound, Upper_Bound)


# Read in example dataframe with direction one way only.
#df_example <- data.frame(Parameter = paste0("Parameter_0",1:9),
#                         Lower_Bound = runif(n = 9, min = 0, max = 10000),
#                         Upper_Bound = runif(n = 9, min = 10000, max = 20000))

# flip odds - so direction of effect is inverted for odd rows
# df_example[c(1,3,5,7,9),c("Lower_Bound", "Upper_Bound")] <- df_example[c(1,3,5,7,9),c("Upper_Bound", "Lower_Bound")]
# 


# Create plot

tornado<-plot_gg_tornado(df = df_params_tornado,
                base_value = ICER_base_case,
                width = 0.8)

ggsave(plot = tornado, filename = "report/images/tornado.png", 
       width = 20, height = 16, units = "cm")


tornado_table <-  arrange(df_params_tornado, desc(abs(Lower_Bound-Upper_Bound)))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 13.1 - Two-way sensitivity analysis, + eligibility for surgery =====

list_eligibility <- c(0.98, 0.95, 0.90, 0.85)

for (elig in list_eligibility){
  
  paste0("eligibility = ", as.character(elig*100),"%")
  
  print( paste0("eligibility for curative intent surgery = ", as.character(elig*100),"%")  )
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  sa_input_outc_chemo_rt <- c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
  sa_input_outc_surgery  <-  c(0.75, 0.77, 0.79, 0.81, 0.83, 0.85, 0.87)
  
  i_max <- length(sa_input_outc_chemo_rt)
  j_max <- length(sa_input_outc_surgery)
  
  v_outc_chemort <- NULL 
  v_outc_surgery <- NULL
  v_icer         <- NULL
  v_incr_cost    <- NULL
  v_incr_qaly    <- NULL
  
  for (i in 1:i_max) {  # chemotherapy
    
    sa_chemo_rt <- sa_input_outc_chemo_rt[i]
    
    for (j in 1:j_max)
    {
      v_outc_chemort <- c(v_outc_chemort, sa_chemo_rt )
      
      sa_surgery  <- sa_input_outc_surgery[j]
      v_outc_surgery <- c(v_outc_surgery, sa_surgery )
      
      # message(paste(i,j))
      
      prop_eligibility_surgery <- elig 
      
      m_TR_SA_soc <- eem(1, 
                         prop_start_soc,
                         prop_males,
                         n_age_init, 
                         
                         
                         smr_dfs_all,
                         smr_dostar,
                         smr_dfs,
                         smr_dfs1,
                         smr_dfs2,
                         smr_rd,    
                         
                         u_Dostar,      
                         u_chemoRT,     
                         u_DFS,    
                         u_DFS1,      
                         u_Surg1,     
                         u_Surg2,     
                         u_DFS2,      
                         u_RD,       
                         u_RM,      
                         u_RM1,       
                         u_RM2,
                         
                         
                         sa_chemo_rt,
                         prop_eligibility_surgery,
                         sa_surgery,
                         sa_surgery,
                         
                         zero,
                         n_dostar_cycles, 
                         c_dfs_per_cycle,
                         c_dfs1_per_cycle,
                         c_dfs2_per_cycle,
                         c_chemoRT_per_cycle,
                         c_surgery,
                         c_rd_per_cycle,
                         c_rmd_per_cycle,
                         c_rmd1_per_cycle,
                         c_rmd2_per_cycle) 
      
      QALY_SA_soc <- m_TR_SA_soc[[3]]
      cost_SA_soc <- m_TR_SA_soc[[4]]
      
      prop_eligibility_surgery <- elig 
      
      m_TR_SA_dostar <- eem(1, 
                            prop_start_dostar,
                            prop_males,
                            n_age_init, 
                            
                            smr_dfs_all,
                            smr_dostar,
                            smr_dfs,
                            smr_dfs1,
                            smr_dfs2,
                            smr_rd,              
                            
                            u_Dostar,      
                            u_chemoRT,     
                            u_DFS,     
                            u_DFS1,      
                            u_Surg1,     
                            u_Surg2,     
                            u_DFS2,      
                            u_RD,       
                            u_RM,      
                            u_RM1,       
                            u_RM2,
                            
                            sa_chemo_rt,
                            prop_eligibility_surgery,
                            sa_surgery,
                            sa_surgery,
                            
                            c_dostar_per_cycle,
                            n_dostar_cycles, 
                            c_dfs_per_cycle,
                            c_dfs1_per_cycle,
                            c_dfs2_per_cycle,
                            c_chemoRT_per_cycle,
                            c_surgery,
                            c_rd_per_cycle,
                            c_rmd_per_cycle,
                            c_rmd1_per_cycle,
                            c_rmd2_per_cycle
                            )
      
      QALY_SA_dostar <- m_TR_SA_dostar[[3]]
      cost_SA_dostar <- m_TR_SA_dostar[[4]]
      ICER <- (cost_SA_dostar- cost_SA_soc)/(QALY_SA_dostar-QALY_SA_soc)
      
      v_incr_cost <- c(v_incr_cost, cost_SA_dostar- cost_SA_soc)
      v_incr_qaly <- c(v_incr_qaly, QALY_SA_dostar-QALY_SA_soc)
      v_icer <- c(v_icer, ICER)
    }
    
  }
  
  df_2way_sa <- data.frame(
    v_outc_chemort, 
    v_outc_surgery,
    v_incr_qaly, 
    v_incr_cost,
    v_icer
  )
  
  df <- df_2way_sa[,c("v_outc_chemort","v_outc_surgery", "v_icer")]
  
  
  
  # write_xlsx(df_2way_sa, "outputs/5-two-way-sa-vs-efficacy-chemort-efficacy-surgery.xlsx")
  
  
  df <- df_2way_sa
  
  df$crt<- paste(round(df$v_outc_chemort*100,0),"%", sep="")
  df$sur<- paste(round(df$v_outc_surgery*100,0),"%", sep="")
  df$ICER <- round(df$v_icer,-2)
  
  df_1 <-subset(df, select=c(crt, sur, ICER))
  
  # Heatmap 
  
  two_way_sa_1 <- ggplot(df_1, aes(x=sur, y=crt)) +
    geom_tile(aes(fill= ICER), color="gray")+
    scale_fill_gradient(low="yellow", high="red")+
    geom_text(data = df_1,
              mapping = aes(x = sur, y = crt, label = 
                              paste0("£", as.character(format(round(ICER/1000,0), 
                                                              nsmall=0, big.mark=",")), 
                                     "k", sep="")
              ))+
    labs(x="% DFS after surgery", y="% DFS after chemo RT") +
    theme(axis.title.x = element_text(size = 10))+
    theme(axis.title.y = element_text(size = 10))+
    theme(legend.text = element_text(size = 10)) 
  
  # two_way_sa_1
  
  f_name <- paste0("report/images/two_way_sa_1-eligibility_",as.character(elig*100),".png")
  ggsave(two_way_sa_1, filename=f_name,
         width = 20, height = 12, units = "cm")
  
  df <- df_2way_sa
  df$sur <- df$v_outc_surgery*100
  df$crt<- paste(round(df$v_outc_chemort*100,0),"%", sep="")
  df$ICER <- round(df$v_icer,-2)
  
  df_2 <-subset(df, select=c(crt, sur, ICER))
  

  two_way_line_plot<-df_2 %>%
    ggplot(aes(x=sur, y=ICER , color=crt))+
    theme_bw()+
    geom_line(linewidth = 1.5)+
    xlab("Disease-free survival after surgery, %")+
    ylab("ICER, £ per QALY")+
    labs(colour = "cCR after ChemoRT")+
    labs(title = "Two-way sensitivity analysis: ICER vs. Surgery & Chemoradiotherapy outcomes")
  
  # two_way_line_plot
  
  f_name <- paste0("report/images/two_way_sa_1_lineplot-eligibility_",as.character(elig*100),".png")
  
   ggsave(two_way_line_plot, filename=f_name,
         width = 20, height = 12, units = "cm")
  # End of loop for surgery and chemo RT 
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
} # end of loop for eligibility 


# write packages we have used to a packages.bib file so that we can reference them later.
knitr::write_bib(x = c(.packages()),
                 file = "report/packages.bib")


# Transform tornado table for presentation 

tornado_table$Lower_Bound <- paste("£", as.character(format(round(as.numeric(tornado_table$Lower_Bound), 0), nsmall=0, big.mark=","))) 
tornado_table$Upper_Bound <- paste("£", as.character(format(round(as.numeric(tornado_table$Upper_Bound), 0), nsmall=0, big.mark=","))) 


# 14. - Render model outputs to report  ========
rmarkdown::render(input = "report/report.Rmd",
                  params = list(
                    tornado_tbl = tornado_table,
                    life_tables = plot_life_tables,
                    table_smr = kbl_inputs_mortality,
                    table_probs = probs_table,    # Data frame
                    fixed_inputs = c("discount_rate_cost" = d_r_cost,
                                     "discount_rate_outc" = d_r_outc,
                                     "time_horizon" = horizon,
                                     "willingness_to_pay" = wtp,
                                     "agestart"=n_age_init),
                    table_inputs_smr = kbl_inputs_mortality,
                    table_inputs_probs = kbl_inputs_probs,
                    
                    table_utils = model_inputs_utilities,
                    
                    table_base_case = c("cost_soc" = cost_soc,
                                        "cost_dostar" = cost_dostar,
                                        "cost_incr" = cost_incr,
                                        
                                        "qaly_soc" = qaly_soc,
                                        "qaly_dostar" = qaly_dostar,
                                        "qaly_incr" = qaly_incr,
                                        
                                        "ICER_str" = ICER_str,
                                        
                                        "tbl_cost_breakdown_dostar_soc" = tbl_cost_breakdown_dostar_soc,
                                        "tbl_cost_breakdown_dostar_dostar" = tbl_cost_breakdown_dostar_dostar,
                                        "tbl_cost_breakdown_dostar_incr" = tbl_cost_breakdown_dostar_incr,
                                        
                                        "tbl_cost_breakdown_dfsall_dostar" = tbl_cost_breakdown_dfsall_dostar,
                                        "tbl_cost_breakdown_dfsall_soc" = tbl_cost_breakdown_dfsall_soc,
                                        "tbl_cost_breakdown_dfsall_incr" = tbl_cost_breakdown_dfsall_incr,
                                        
                                        "tbl_cost_breakdown_ChemoRT_dostar" = tbl_cost_breakdown_ChemoRT_dostar,
                                        "tbl_cost_breakdown_ChemoRT_soc" = tbl_cost_breakdown_ChemoRT_soc,
                                        "tbl_cost_breakdown_chemoRT_incr" = tbl_cost_breakdown_chemoRT_incr,
                                        
                                        
                                        "tbl_cost_breakdown_surgery_dostar" = tbl_cost_breakdown_surgery_dostar,
                                        "tbl_cost_breakdown_surgery_soc" = tbl_cost_breakdown_surgery_soc,
                                        "tbl_cost_breakdown_surgery_incr" = tbl_cost_breakdown_surgery_incr,
                                        
                                        "tbl_cost_breakdown_rd_dostar" = tbl_cost_breakdown_rd_dostar,
                                        "tbl_cost_breakdown_rd_soc" = tbl_cost_breakdown_rd_soc,
                                        "tbl_cost_breakdown_rd_incr" = tbl_cost_breakdown_rd_incr,
                                        
                                        "tbl_cost_breakdown_all_rmd_dostar" = tbl_cost_breakdown_all_rmd_dostar,
                                        "tbl_cost_breakdown_all_rmd_soc" = tbl_cost_breakdown_all_rmd_soc,
                                        "tbl_cost_breakdown_all_rmd_incr" = tbl_cost_breakdown_all_rmd_incr,
                                        
                                        "lm_surv_year_1_soc" = lm_surv_year_1_soc, 
                                        "lm_surv_year_2_soc" = lm_surv_year_2_soc,
                                        "lm_surv_year_3_soc" = lm_surv_year_3_soc,
                                        "lm_surv_year_5_soc" = lm_surv_year_5_soc,
                                        
                                        "lm_surv_year_1_dostar" = lm_surv_year_1_dostar,
                                        "lm_surv_year_2_dostar" = lm_surv_year_2_dostar,
                                        "lm_surv_year_3_dostar" = lm_surv_year_3_dostar,
                                        "lm_surv_year_5_dostar" = lm_surv_year_5_dostar,
                                        
                                        "lm_surv_year_1_incr" = lm_surv_year_1_incr,
                                        "lm_surv_year_2_incr" = lm_surv_year_2_incr,
                                        "lm_surv_year_3_incr" = lm_surv_year_3_incr,
                                        "lm_surv_year_5_incr" = lm_surv_year_5_incr,
                                        
                                        "tbl_QALY_breakdown_dostar_soc" = tbl_QALY_breakdown_dostar_soc, 
                                        "tbl_QALY_breakdown_dostar_dostar" = tbl_QALY_breakdown_dostar_dostar, 
                                        "tbl_QALY_breakdown_dostar_incr" = tbl_QALY_breakdown_dostar_incr, 
                                        
                                        "tbl_QALY_breakdown_dfs_soc" = tbl_QALY_breakdown_dfs_soc, 
                                        "tbl_QALY_breakdown_dfs_dostar" = tbl_QALY_breakdown_dfs_dostar, 
                                        "tbl_QALY_breakdown_dfs_incr" = tbl_QALY_breakdown_dfs_incr, 
                                        
                                        "tbl_QALY_breakdown_rm_soc" = tbl_QALY_breakdown_rm_soc, 
                                        "tbl_QALY_breakdown_rm_dostar" = tbl_QALY_breakdown_rm_dostar, 
                                        "tbl_QALY_breakdown_rm_incr" = tbl_QALY_breakdown_rm_incr, 
                                        
                                        "tbl_QALY_breakdown_chemoRT_soc" = tbl_QALY_breakdown_chemoRT_soc,           
                                        "tbl_QALY_breakdown_chemoRT_dostar" = tbl_QALY_breakdown_chemoRT_dostar,
                                        "tbl_QALY_breakdown_chemoRT_incr" = tbl_QALY_breakdown_chemoRT_incr,
                                        
                                        "tbl_QALY_breakdown_dfs1_soc" = tbl_QALY_breakdown_dfs1_soc,
                                        "tbl_QALY_breakdown_dfs1_dostar" = tbl_QALY_breakdown_dfs1_dostar,
                                        "tbl_QALY_breakdown_dfs1_incr" = tbl_QALY_breakdown_dfs1_incr,
                                        
                                        "tbl_QALY_breakdown_surgery_soc" = tbl_QALY_breakdown_surgery_soc, 
                                        "tbl_QALY_breakdown_surgery_dostar" = tbl_QALY_breakdown_surgery_dostar,
                                        "tbl_QALY_breakdown_surgery_incr" = tbl_QALY_breakdown_surgery_incr,
                                        
                                        "tbl_QALY_breakdown_rm1_soc" = tbl_QALY_breakdown_rm1_soc,      
                                        "tbl_QALY_breakdown_rm1_dostar" = tbl_QALY_breakdown_rm1_dostar,         
                                        "tbl_QALY_breakdown_rm1_incr" = tbl_QALY_breakdown_rm1_incr,
                                        
                                        "tbl_QALY_breakdown_dfs2_soc" = tbl_QALY_breakdown_dfs2_soc,           
                                        "tbl_QALY_breakdown_dfs2_dostar" = tbl_QALY_breakdown_dfs2_dostar,
                                        "tbl_QALY_breakdown_dfs2_incr" = tbl_QALY_breakdown_dfs2_incr,                                        
                                        
                                        "tbl_QALY_breakdown_rd_soc" = tbl_QALY_breakdown_rd_soc,
                                        "tbl_QALY_breakdown_rd_dostar" = tbl_QALY_breakdown_rd_dostar,       
                                        "tbl_QALY_breakdown_rd_incr" = tbl_QALY_breakdown_rd_incr, 
                                        
                                        "tbl_QALY_breakdown_rm2_soc" = tbl_QALY_breakdown_rm2_soc,            
                                        "tbl_QALY_breakdown_rm2_dostar" = tbl_QALY_breakdown_rm2_dostar,   
                                        "tbl_QALY_breakdown_rm2_incr" = tbl_QALY_breakdown_rm2_incr
                                        
                    ),
                    utils = c(
                      "u_Dostar" = as.character(round(u_Dostar,3),nsmall=3),
                      "u_DFS" = as.character(round(u_DFS,3),nsmall=3),
                      "u_DFS1" = as.character(round(u_DFS1,3),nsmall=3),
                      "u_DFS2" = as.character(round(u_DFS2,3),nsmall=3),
                      "u_RM" = as.character(round(u_RM,3),nsmall=3),
                      "u_chemoRT" = as.character(round(u_chemoRT,3),nsmall=3),
                      "u_Surg1" = as.character(round(u_Surg1,3),nsmall=3),
                      "u_Surg2" = as.character(round(u_Surg2,3),nsmall=3),
                      
                      "u_RD" = as.character(round(u_RD,3),nsmall=3),
                      "u_RM1" = as.character(round(u_RM1,3),nsmall=3),
                      "u_RM2" = as.character(round(u_RM2,3),nsmall=3)
                    ),
                    
                    plots = c(
                      "plot_OS" = plot_OS
                    ),
                    
                    
                    probs = c(
                      
                      "p_dostar_efficacy" = paste0(as.character(p_dostar_efficacy*100),"%"),
                      "prop_chemort_dfs1" = paste0(as.character(prop_chemort_dfs1*100),"%"),
                      "prop_surg1_dfs2" = paste0(as.character(prop_surg1_dfs2*100),"%"),
                      "prop_surg2_dfs2" = paste0(as.character(prop_surg2_dfs2*100),"%"),
                      "ELG" = paste0(as.character(ELG*100),"%"),
                      
                      "prop_dfs_to_surgery" = paste0(as.character(prop_dfs_to_surgery*100),"%"),
                      "p_d_chemort" = paste0(as.character(p_d_chemort*100),"%")
                    ),
                    
                    hcru_costs = c(
                      "c_dostar_per_cycle"  = paste0("£",as.character(round(c_dostar_per_cycle,0))),
                      "n_dostar_cycles"     = as.character(n_dostar_cycles),           
                      "c_dfs_per_cycle"     = paste0("£",as.character(round(c_dfs_per_cycle,0))),         
                      "c_dfs1_per_cycle"    = paste0("£",as.character(round(c_dfs1_per_cycle,0))),   
                      "c_dfs2_per_cycle"    = paste0("£",as.character(round(c_dfs2_per_cycle,0))), 
                      "c_chemoRT_per_cycle" = paste0("£",as.character(round(c_chemoRT_per_cycle,0))),    
                      "c_surgery"           = paste0("£",as.character(round(c_surgery,0))),            
                      "c_rd_per_cycle"      = paste0("£",as.character(round(c_rd_per_cycle ,0))),      
                      "c_rmd_per_cycle"     = paste0("£",as.character(round(c_rmd_per_cycle,0))),      
                      "c_rmd1_per_cycle"    = paste0("£",as.character(round(c_rmd1_per_cycle,0))),        
                      "c_rmd2_per_cycle"    = paste0("£",as.character(round(c_rmd2_per_cycle,0)))
                    )
                    
                  ))
