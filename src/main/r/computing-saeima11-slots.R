# Compute # Of candidates 
# in a situation, where 11th Saeima 
# would be elected by their actual number

voters <- c(282337,260506,115486,118200,131685)
theSum <- sum(voters)/100
voters/theSum
slots <- c(31,29,13,13,14)
round(voters/slots)

slots <- c(31,29,12,13,15)
round(voters/slots)




