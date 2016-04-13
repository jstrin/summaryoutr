####################
#
# Functions to create a dataframe with two summary stats
#  across a variety of variables
#
#   inputs:
#     df - a dataframe *required*
#     stat1 - first summary statistic
#     stat2 - second summary statistic
#     l.variables - list of variables to summarise
#     l.grouping - list of variables to group by
#
#   output:
#     a data frame with two columns for each varialbe in l.variables
#         one for each summary statistic
####################




summarize_df <- function(df, stat1='Average', stat2='Count',
                         l.variables = 'all.summary', l.grouping = 'first.summary'){

  # If no grouping variable(s) are specified, use the first column

  if(l.grouping == 'first.summary'){
    l.grouping <- names(df)[-1]
  }

  # if no variables to summarise are specified, use all but the grouping variables

  if(l.variables == 'all.summary'){
   l.variables <- names(df)[-which(names(df) %in% l.grouping)]
  }

  # Create the summary functions

stat.func1 <- summaryStatIdentifier(stat1)
stat.func2 <- summaryStatIdentifier(stat2)

  # create an empty data frame
df_out <- data.frame()

  # for each varialbe in the list, summarise using the two summary stats

for(i in l.variables){
  df_temp <- df %>%
    select_(l.grouping, i) %>%
    group_by_( l.grouping ) %>%
    summarise_each(funs(stat1 = stat.func1(.), stat2 = stat.func2(.)) )

  # rename the data frame using the user supplied summary statistic identifiers and the
  #  variable names so that names will be human-readable
  names(df_temp) <- c(l.grouping, paste(i, stat1, sep = "_"), paste(i, stat2, sep = "_"))


# To do:
#   - replace the if(i in l.var...) by changing the data frame creation
#   - add in conditional to create stat over n capability for output


  # if the variable is the first one, replace df_out with df_temp, otherwise, join together
  #   ddataframes for output

  if(i %in% l.variables[1]){
    df_out <- df_temp
  }else{
  df_out <- full_join(df_out, df_temp, by = l.grouping)
  }
}

df_out
}



################
#
# Create a function to define the summary
#  stat aliases
#
# Returns a function
#
####################

summaryStatIdentifier <- function(summary.stat){
  if( grepl("count", summary.stat, ignore.case = T)){
    stat.summariser <- function(.){
      sum(!is.na(.))
    }
  }else{
  if(grepl("Average", summary.stat, ignore.case = T) |
     grepl("mean", summary.stat, ignore.case = T) |
     grepl("ave", summary.stat, ignore.case = T)){
    stat.summariser <- function(.){
      mean(., na.rm = T)
    }
  }else{
    if(grepl("Median", summary.stat, ignore.case = T) |
       grepl("Med", summary.stat, ignore.case = T)){
      stat.summariser <- function(.){
        median(., na.rm = T)
      }
    }
  }
  }
  stat.summariser
}
