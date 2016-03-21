import resource
lista = [line.rstrip('\n') for line in open('IntegerArray.txt')]
for i in range(0,len(lista)):
    lista[i]=int(lista[i]) 

def merge_grande(array):
    meio=len(array)//2
    if len(array)<2:
        return(array)
    esquerda=array[:meio]
    direita=array[meio:]
    resultado=merge_pequeno(merge_grande(esquerda),merge_grande(direita))    
    return(resultado)


inversoes = 0    

def merge_pequeno(arrayE,arrayD):
    i=0
    j=0
    resposta=[]  
    global inversoes
    while i<len(arrayE) and j<len(arrayD):
        if arrayE[i]>arrayD[j]:
            resposta.append(arrayD[j])
            inversoes = inversoes + (len(arrayE) - i)
            j=j+1
        else: 
            resposta.append(arrayE[i])
            i=i+1
    resposta.extend(arrayE[i:]) 
    resposta.extend(arrayD[j:])
        
    return(resposta)        

def limit_memory(maxsize):
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    resource.setrlimit(resource.RLIMIT_AS, (maxsize, hard))

merge_grande(lista)
print inversoes
