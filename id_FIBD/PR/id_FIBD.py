#!/usr/bin/env python
def fibmort(n,k,m):
    months=n
    offsprings=k
    lifespan=m

    baby_list=[0]*lifespan
    mature_list=[0]*lifespan
    total_bunnies_list=[0]*lifespan
    
    
    for month in range(1,months+1):
        if month == 1:
            babies = 1
            mature = 0
            total_bunnies = mature + babies
        
            baby_list.append(babies)
            mature_list.append(mature)
            total_bunnies_list.append(total_bunnies)
        else:
            mature = (mature_list[-1]+baby_list[-1]-baby_list[-(lifespan)])
            babies = offsprings*mature_list[-1]
            
            # update baby and mature list for this generation
            baby_list.append(babies)
            mature_list.append(mature)
        
            # calculate total_bunnies for this generation
            total_bunnies = mature_list[-1] + baby_list[-1]
            total_bunnies_list.append(total_bunnies)

    return(total_bunnies_list[-1])

