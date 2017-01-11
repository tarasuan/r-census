# Analysis of Census Data using R

# acs package for R 
# The acs class provides a convenient wrapper for demographic data from the U.S. Census, especially the American Community Survey. Estimates and standard errors are kept together, along with geographic information and metadata necessary to manipulate and analyze data in this form.
library(acs)
library(ggplot2)

# set united states variable
us=geo.make(state=(c(1,2,4:6,8:13, 15:42, 44:51, 53:56)))
us
str(us)

# get proportion of stem/state ()
# Key Stem category is: Computer, engineering, and science occupations. 
# Span is 2010-2015, before 2010 this key category did not exist and was dispersed among other groupings.
# actuall span is fine back to 2005. The issue was sub-cats also changed, previously used B24010, but C24010 is the correct table.

num.var <- c("C24010_007", "C24010_043")
den.var <- c("C24010_001")
national.stem.data <- acs.fetch(endyear = 2015, span = 5, geography = us, variable = append(den.var, num.var))
national.stem.data
numer <- apply(national.stem.data[,num.var],  MARGIN = 2, FUN = sum, agg.term = c("y", "numer"), one.zero = TRUE)
denom <- apply(national.stem.data[,den.var],  MARGIN = 2, FUN = sum, agg.term = c("y", "denom"), one.zero = TRUE)
national.stem.pct <- divide.acs(numer, denom, method = "proportion")
national.stem.pct

national.stem.data.prev <- acs.fetch(endyear = 2010, span = 5, geography = us, variable = append(den.var, num.var))
national.stem.data.prev
numer <- apply(national.stem.data.prev[,num.var],  MARGIN = 2, FUN = sum, agg.term = c("y", "numer"), one.zero = TRUE)
denom <- apply(national.stem.data.prev[,den.var],  MARGIN = 2, FUN = sum, agg.term = c("y", "denom"), one.zero = TRUE)
national.stem.prev.pct <- divide.acs(numer, denom, method = "proportion")
national.stem.prev.pct

#national.stem.set = append(national.stem.pct, national.stem.prev.pct)
#national.stem.set

#prepare chorograph of 2011-2015 pct data
national.stem.pct.est = data.frame(state.stem.geo=geography(national.stem.pct)[[1]], stem.pct.est=as.numeric(estimate(national.stem.pct)))
# turns out the proportion of jobs that comes from tech ranges over the states. DC is number 1, CA is 10.
levels(national.stem.pct.est$state.stem.geo) <- tolower(levels(national.stem.pct.est$state.stem.geo))

# Initialize a state map using map_data
state.df=map_data("state")
choropleth=merge(x=state.df,y=national.stem.pct.est, by.x="region", by.y = "state.stem.geo")
choropleth=choropleth[order(choropleth$order), ]
choropleth$national.stem.pct.est.d=cut(choropleth$stem.pct.est, breaks=c(.01,.02,.03,.04,.05,.06,.07,.08,.09,.1), include.lowest=T)
ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = national.stem.pct.est.d), colour = "white", size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA) +
  scale_fill_brewer(palette = "Purples")

#graph previous pct for good measure
national.stem.prev.pct.est = data.frame(state.stem.geo=geography(national.stem.prev.pct)[[1]], state.pct.est=as.numeric(estimate(national.stem.prev.pct)))
# turns out the proportion of jobs that comes from tech ranges over the states. DC is number 1, CA is 10.
levels(national.stem.prev.pct.est$state.stem.geo) <- tolower(levels(national.stem.prev.pct.est$state.stem.geo))

# Initialize a state map using map_data
state.df2=map_data("state")
choropleth2=merge(x=state.df2,y=national.stem.prev.pct.est, by.x="region", by.y = "state.stem.geo")
choropleth2=choropleth[order(choropleth2$order), ]
choropleth2$national.stem.prev.pct.est.d=cut(choropleth2$stem.pct.est, breaks=c(.01,.02,.03,.04,.05,.06,.07,.08,.09,.1), include.lowest=T)
ggplot(choropleth2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = national.stem.prev.pct.est.d), colour = "white", size = 0.2) + 
  geom_polygon(data = state.df2, colour = "white", fill = NA) +
  scale_fill_brewer(palette = "Purples")

# Make a choropleth of the diff totals
diff.stem.pct
diff.stem.pct.est = data.frame(state.stem.geo=geography(diff.stem.pct)[[1]], diff.stem.pct.est=as.numeric(estimate(diff.stem.pct)))
levels(diff.stem.pct.est$state.stem.geo) <- tolower(levels(diff.stem.pct.est$state.stem.geo))
state.df3=map_data("state")
choropleth3=merge(x=state.df3,y=diff.stem.pct.est, by.x="region", by.y = "state.stem.geo")
choropleth3=choropleth3[order(choropleth3$order), ]
choropleth3$diff.stem.pct.est.d=cut(choropleth3$diff.stem.pct.est, breaks=8, include.lowest=T)
ggplot(choropleth3, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = diff.stem.pct.est.d), colour = "white", size = 0.2) + 
  geom_polygon(data = state.df3, colour = "white", fill = NA) +
  scale_fill_brewer(palette = "Purples")

# determine the significance of the difference
# reference online?
diff.stem.pct=national.stem.pct - national.stem.prev.pct
diff.stem.pct
t.test = (estimate(diff.stem.pct)/standard.error(diff.stem.pct))
t.test
# need to understand these results better. 
# source: http://eglenn.scripts.mit.edu/citystate/2013/11/using-acs-r-for-a-t-test/

# Order the results...
t.test.ordered = t.test[order(-t.test),]
t.test.ordered
# results CA, NC, TX, West V, Washington, Ohio, but in fact, 31 states showed significant increases in the percentage of stem jobs as a proportion of total jobs in these jobs. 
# should aggregate the data so that can see % of STEM jobs, and the Percent Change (or whatever that second thing is)
t.test = setDT(t.test.df, keep.rownames = TRUE)[]
colnames(t.test)[1] <- "state.stem.geo"

# merge data into one dataframe for further analysis
national.stem.pct.df = data.frame(state.stem.geo=geography(national.stem.pct)[[1]], natl.stem.pct.est=as.numeric(estimate(national.stem.pct)))
diff.stem.pct.df = data.frame(state.stem.geo=geography(diff.stem.pct)[[1]], diff.stem.pct.est=as.numeric(estimate(diff.stem.pct)))
total.df <- merge(national.stem.pct.df,diff.stem.pct.df,t.test, by="state.stem.geo")

# or one deeper cut. look at gender. 

#-------------- clear til here ------------
# compute proportion of stem occupations (male)
male.stem.data = acs.fetch(endyear = 2015, span = 1, geography = us, variable = "B24010_019")
# test.pct = divide.acs(test.data[,2],test.data[,1], method = "proportion")

# female
female.stem.data = acs.fetch(endyear = 2015, span = 1, geography = us, variable = B24010_170)
# female.test.pct = divide.acs(female.test.data[,2],female.test.data[,1], method = "proportion")


#the time range(s) are 2005-2009 and 2010-2014
#need to redo adding the two data sets into one acs object. 
# it will do the weird warning? what was it?
# need to calculate the diff / standard dev (see web page)
# the results, once replicated, seemed all over the place, in that
#some values were way way above 1.65 or whatever the 95% is

