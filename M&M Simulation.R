##############################
## M&M Simulation           ##
## Jacob Michel             ##
##############################
library(ggplot2)
library(stringr)
##############################
## Create Simulation        ##
##############################

##################################################
###### Create color probability distribution #####
##################################################
red_prob = 9/129
yellow_prob = 21/129
orange_prob = 34/129
blue_prob = 30/129
green_prob = 20/129
brown_prob = 15/129

simulation.size = 10000
bag_size = 21

# binomial distribution is a large number of 0/1's with a certain probabilties

# Binomial Distribution
red_binom = rbinom(simulation.size, 1, red_prob)
red_binom = rbinom(bag_size, 1, red_prob)
yellow_binom = rbinom(bag_size, 1, yellow_prob)
orange_binom = rbinom(bag_size, 1, orange_prob)
blue_binom = rbinom(bag_size, 1, blue_prob)
green_binom = rbinom(bag_size, 1, green_prob)
brown_binom = rbinom(bag_size, 1, brown_prob)


##############################################################################
##### simulate 10000 bags (where there are integer values of each color) #####
##############################################################################
# will need to created column for each color 10000 X 6 where the sum of col1:col6 = 5
first5 = data.frame(matrix(ncol=6, nrow=simulation.size))
cols = c('red','yellow','orange','blue','green','brown')
colnames(first5) = cols


for (i in 1:simulation.size) {
  # Use binomial distribution to simulate each bag
  red_binom = rbinom(bag_size, 1, red_prob)
  yellow_binom = rbinom(bag_size, 1, yellow_prob)
  orange_binom = rbinom(bag_size, 1, orange_prob)
  blue_binom = rbinom(bag_size, 1, blue_prob)
  green_binom = rbinom(bag_size, 1, green_prob)
  brown_binom = rbinom(bag_size, 1, brown_prob)
  
  # re-assign each MM it's color as a string
  red_binom = replace(red_binom, red_binom==1, 'red')
  yellow_binom = replace(yellow_binom, yellow_binom==1, 'yellow')
  orange_binom = replace(orange_binom, orange_binom==1, 'orange')
  blue_binom = replace(blue_binom, blue_binom==1, 'blue')
  green_binom = replace(green_binom, green_binom==1, 'green')
  brown_binom = replace(brown_binom, brown_binom==1, 'brown')
  
  #remove all instances of failing to form an MM
  red_binom = red_binom[red_binom != '0']
  yellow_binom = yellow_binom[yellow_binom != '0']
  orange_binom = orange_binom[orange_binom != '0']
  blue_binom = blue_binom[blue_binom != '0']
  green_binom = green_binom[green_binom != '0']
  brown_binom = brown_binom[brown_binom != '0']
  
  # Simulate the total bag with all MM's by adding all the colors together
  bag = c(red_binom, yellow_binom, orange_binom, blue_binom, green_binom, brown_binom)
  
  # sample 5 from the bag
  pull5 = sample(bag, 5)
  
  # count how many of each color
  first5$red[i] = sum(str_count(pull5, 'red'))
  first5$yellow[i] = sum(str_count(pull5, 'yellow'))
  first5$orange[i] = sum(str_count(pull5, 'orange'))
  first5$blue[i] = sum(str_count(pull5, 'blue'))
  first5$green[i] = sum(str_count(pull5, 'green'))
  first5$brown[i] = sum(str_count(pull5, 'brown'))
  
}


#########################################################################
##### determine the occurence of 1,2...5 of the same  and fullhouse #####
##### Full house is 3 of a kind and 2 of a kind                     #####
#########################################################################
alldiff = subset(first5, ((red < 2)  & (orange < 2) & (yellow < 2) & (blue < 2) & (green < 2) & (brown < 2)))
same2 = subset(first5, ( ((red == 2) | (orange == 2) | (yellow == 2) | (blue == 2) | (green == 2) | (brown == 2)) & ((red < 3) & (orange < 3) & (yellow < 3) & (blue < 3) & (green < 3) & (brown < 3)) ) )
same3 = subset(first5, ((red == 3) | (orange == 3) | (yellow == 3) | (blue == 3) | (green == 3) | (brown == 3)))
same4 = subset(first5, ((red == 4) | (orange == 4) | (yellow == 4) | (blue == 4) | (green == 4) | (brown == 4)))
same5 = subset(first5, ((red == 5) | (orange == 5) | (yellow == 5) | (blue == 5) | (green == 5) | (brown == 5)))
fullhouse = subset(first5, ( ((red == 3)  | (orange == 3) | (yellow == 3) | (blue == 3) | (green == 3) | (brown == 3)) & ((red == 2)  | (orange == 2) | (yellow == 2) | (blue == 2) | (green == 2) | (brown == 2)) ))

# count all the instances
ct_alldiff = nrow(alldiff)
ct_same2 = nrow(same2)
ct_same3 = nrow(same3)
ct_same4 = nrow(same4)
ct_same5 = nrow(same5)
ct_fullhouse = nrow(fullhouse)

# check to make sure it equals 10000
total_check = sum(ct_alldiff, ct_same2, ct_same3, ct_same4, ct_same5)
print(total_check)

# form final dataset for the occurences of each outcome
occurences = data.frame(matrix(ncol=6, nrow=1))
cols_occur = c('AllDiff','Same2','Same3','Same4','Same5','FullHouse')
colnames(occurences) = cols_occur

occurences$AllDiff = ct_alldiff
occurences$Same2 = ct_same2
occurences$Same3 = ct_same3
occurences$Same4 = ct_same4
occurences$Same5 = ct_same5
occurences$FullHouse = ct_fullhouse

instances = colnames(occurences)

# transpose dataset
occurences2 = data.frame(matrix(ncol=2, nrow=6))

occurences2$X1 = t(occurences[1,])
occurences2$X2 = cols_occur
names(occurences2) <- c("Frequency", "Instance")
         
# plot                 
ggplot(occurences2, aes(x = Instance, y = Frequency, label=Frequency, fill = Instance)) + 
  geom_bar(stat='identity') + geom_text(nudge_y = 300) + ggtitle("Frequency of Occurence for Pulling 5 M&M's Out of a Bag")



