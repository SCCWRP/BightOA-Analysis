# OAanalysis

Performs the pH correction on the bottle dataframe by getting pH at insitu, using the pHinsi function from the seacarb library.

Then it gets the delta pH, the difference between the CTD data pH and the bottle pH (at insitu), at the corresponding depths

After that, it calculates the linear regression equation, plotting depth as 'x' and delta pH as 'y' in order to predict what the delta pH would be at certain depths, so that we can "fill in the blanks" per se, in the CTD dataframe where we were not able to calculate a true delta pH. Therefore we predict what it would be using linear regression.

After that, we subtract the delta pH from the reported pH values in the CTD dataframe, to get the 'corrected pH'

Then we compute the omega values in the CTD and Bottle dataframes respectively, using the carb function from the seacarb library

The 'shiny' branch of this repository is NOT meant to be edited, merged or anything like that. It needs to be kept as is for the sake of being able to put this code in Nick's old shiny app. This is a temporary fix before upgrading the checker application. Most likely by 'dockerizing' everything so that dependency issues are not a problem.

I tried to mess with the code after it was already in a state of being compatible with the shiny app, and sure enough i broke it and wasted hours of precious time trying to figure out what i did. I created the shiny branch to save a particular working state of the app, while being able to edit on other branches.

