library(phydynR)

demes <- c( 'E', 'Il', 'Ih', 'exog' )
nondemes <- c( 'R', 'S', 'infections' )

parms <- list(
   gamma0 = 73.0
 , gamma1 = 121.667
 , gammaExog = 44.0
 , p_h = 0.2
 , b = 15.0
 , tau = 73.98393
 , exogGrowthRate = 36.5
 , importRate = 5.0
)

x0 <- c( E = 1.0, Il = 1.0E-8, Ih = 1.0E-8, exog = 0.05, R = 0.0, S = 2.05E7, infections = 0.0)

births <- matrix('0', nrow=4, ncol=4)
rownames(births)=colnames(births) <- demes

migs <- matrix('0', nrow=4, ncol=4)
rownames(migs)=colnames(migs) <- demes

deaths <- rep(0,4)
names(deaths) <- demes 

nonDemeDynamics <- c() 

migs['E','Il'] = '(if (((t>2020.085)))  (parms$gamma0*E*(1-parms$p_h)) else (0.0))';
migs['E','Ih'] = '(if (((t>2020.085)))  (parms$gamma0*E*parms$p_h) else (0.0))';
births['Il','E'] = '((if (((t>2020.085)))  (max(0.0,parms$b)) else (0.0)))*Il*S/(S+Il+Ih+E+R)';
births['Ih','E'] = '((if (((t>2020.085)))  (max(0.0,parms$b)) else (0.0)))*Ih*parms$tau*S/(S+Il+Ih+E+R)';
deaths['Il'] = 'parms$gamma1*Il';
deaths['Ih'] = 'parms$gamma1*Ih';
nonDemeDynamics['R'] = 'parms$gamma1*(Il+Ih)';
nonDemeDynamics['S'] = '-((if (((t>2020.085)))  (max(0.0,parms$b)) else (0.0)))*(Ih*parms$tau+Il)*S/(S+Il+Ih+E+R)';
nonDemeDynamics['infections'] = '((if (((t>2020.085)))  (max(0.0,parms$b)) else (0.0)))*(Ih*parms$tau+Il)*S/(S+Il+Ih+E+R)';
migs['exog','Il'] = 'parms$importRate*Il';
migs['Il','exog'] = 'parms$importRate*Il';
migs['exog','Ih'] = 'parms$importRate*Ih';
migs['Ih','exog'] = 'parms$importRate*Ih';
migs['exog','E'] = 'parms$importRate*E';
migs['E','exog'] = 'parms$importRate*E';
births['exog','exog'] = '(parms$gammaExog+parms$exogGrowthRate)*exog';
deaths['exog'] = 'parms$gammaExog*exog';


dm <- build.demographic.process( births = births
 , migrations = migs 
 , deaths = deaths 
 , nonDemeDynamics = nonDemeDynamics
 , parameterNames = names(parms)
 , sde = FALSE
 , rcpp = FALSE
)


#t0t1 <- list(t0 = 2019.7 , t1 = 2020.3 )


#show.demographic.process(dm
# , theta = parms
# , x0 = x0
# , t0 = t0t1$t0 
# , t1 = t0t1$t1
# , integrationMethod = 'rk4' , res = 1000
#)


#dm.run <- dm(theta = parms
# , x0 = x0
# , t0 = t0t1$t0 
# , t1 = t0t1$t1 
# , integrationMethod = 'rk4' , res = 1000
#)

