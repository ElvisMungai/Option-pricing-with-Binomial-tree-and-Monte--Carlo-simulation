xyz=read.csv('Implied Volatility.csv')
T=30/365
S=10652.5
rf=0.065
#k=11000
option_price=0
q=0 #Dividend yield assumption

#Number of steps
n=20
delt=T/n
error=0.5

#Implied Volatility Calculation

for(w in 1:31){
  
if(w<=14){error=10}
if(w>14 && w<=23){error=2}
if(w>23){error=0.5}
  
sig=0.0001
k=xyz$Strike.Price[w]
while(abs(xyz$Settle.Price[w]-option_price)>error)
  {
    print(option_price)
    #Calculate the constants
    
    u=exp(sig*sqrt(delt))
    d=1/u
    a=exp((rf-q)*delt)
    p=(a-d)/(u-d)
    
    #Possible values matrix
    
    price=matrix(nrow = n+1,ncol = n+1)
    price[1,1]=S
    for(col in 1:n)
    {
      price[1,col+1]= price[1,col]*u
      
      for(row in 1:col){
        price[row+1,col+1]=price[row,col]*d
        
      }
    }
    
    #Payoff Matrix
    
    i=1
    payoff=matrix(nrow = n+1,ncol = n+1)
    
    for(i in 1:(n+1))
    {
      payoff[i,n+1]= max(price[i,n+1]-k,0)
    }
    
    
    for(j in 1:n){
      for(i in 1:(n+1-j))
      {
        payoff[i,n+1-j]= exp(-rf*delt)*{p*payoff[i,n-j+1+1] + (1-p)*payoff[i+1,n-j+1+1]}
      }
    }
    option_price = payoff[1,1]
    
    sig= sig + 0.0001
print(sig)
}
xyz$implied_volatility[w]=sig 
}

scatter.smooth(xyz$Strike.Price,xyz$implied_volatility,xlab = 'Strike Price', ylab = 'Implied Volatility', col='blue',ylim = range(0.15,0.2))











