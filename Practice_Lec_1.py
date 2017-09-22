
# coding: utf-8

# In[ ]:

# Python Program to Print all Numbers in a Range Divisible by a Given Number 

lower=int(input("Enter lower range limit:"))
upper=int(input("Enter upper range limit:"))
n=int(input("Enter the number to be divided by:"))
for i in range(lower,upper+1):
    if(i%n==0):
        print(i)       


# In[ ]:

# Python Program to Read a Mumber n and Compute n+nn+nnn 

n= input("Enter a number n: ")
temp= str(n)
t1 = temp+temp
t2 = temp+temp+temp
comp = int(n)+int(t1)+int(t2)
print("The value is:", comp)   


# In[ ]:

# Python Program to Print Largest Even and Largest Odd Number in a List

n = int(input("Enter the number of elements:"))
odd = []
even = []
for i in range(0,n):
    if(i%2 == 0):
        even.append(i)
    else:
        odd.append(i)
l_even = even[len(even)-1]
l_odd = odd[len(odd)-1]
print("The largest Even and Odd number in the list is %d and %d respectively",(l_even, l_odd))


# In[ ]:

#Python Program to Find the Largest and the second largest Number in a List
n = int(input("Enter the number of elements:"))
a = []
for i in range(0,n):
    a.append(i)
a.sort()
l=  a[len(a)-1]
sl=  a[len(a)-2]

print("The largest number in list a is %d" %l)
print("The second largest number in list a is %d" %sl)


# In[ ]:

#Python Program to Print Table of a Given Number 
n = int(input("Enter a number:"))
for i in range(1,11):
    if(n > 0):
        print(n,"x",i,"=",n*1)
    else:
        print("Enter a positive value")

#2
n=int(input("Enter the number to print the tables for:"))
for i in range(1,11):
    print(n,"x",i,"=",n*i)


# In[ ]:

# Python Program to find the smallest divisor of an integer
n = int(input("Enter a number:"))
a = []
for i in range(1,n+1):
    if(n%i==0):
        a.append(i)
    a.sort()
print("The smallest diviser of interger %d is %d" %(n,a[0]))


# In[ ]:

# Python Program to Read Height in Centimeters and then Convert the Height to Feet and Inches
cm=int(input("Enter the height in centimeters:"))
inches=0.394*cm
feet=0.0328*cm
print("The length in inches",round(inches,2))
print("The length in feet",round(feet,2))


# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:



