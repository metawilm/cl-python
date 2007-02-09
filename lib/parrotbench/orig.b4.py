sample = '''

def heappush(heap, item):
    heap.append(item)
    _siftdown(heap, 0, len(heap)-1)

def heappop(heap):
    lastelt = heap.pop()    # raises appropriate IndexError if heap is empty
    if heap:
        returnitem = heap[0]
        heap[0] = lastelt
        _siftup(heap, 0)
    else:
        returnitem = lastelt
    return returnitem

def heapreplace(heap, item):
    returnitem = heap[0]    # raises appropriate IndexError if heap is empty
    heap[0] = item
    _siftup(heap, 0)
    return returnitem

def heapify(x):
    n = len(x)
    for i in xrange(n//2 - 1, 0-1, 0-1):
        _siftup(x, i)

def _siftdown(heap, startpos, pos):
    newitem = heap[pos]
    while pos > startpos:
        parentpos = (pos - 1) >> 1
        parent = heap[parentpos]
        if parent <= newitem:
            break
        heap[pos] = parent
        pos = parentpos
    heap[pos] = newitem

def _siftup(heap, pos):
    endpos = len(heap)
    startpos = pos
    newitem = heap[pos]
    # Bubble up the smaller child until hitting a leaf.
    childpos = 2*pos + 1    # leftmost child position
    while childpos < endpos:
        # Set childpos to index of smaller child.
        rightpos = childpos + 1
        if rightpos < endpos:
            if heap[rightpos] <= heap[childpos]:
                childpos = rightpos
        # Move the smaller child up.
        heap[pos] = heap[childpos]
        pos = childpos
        childpos = 2*pos + 1
    # The leaf at pos is empty now.  Put newitem there, and bubble it up
    # to its final resting place (by sifting its parents down).
    heap[pos] = newitem
    _siftdown(heap, startpos, pos)
'''

from b0 import Parser, Scanner, getcFromString, Node, eval, Dict
from b0 import instrumentTree, unInstrumentTree, output, checkoutput

def main():
    output.reset()
    scanner = Scanner(getcFromString(sample).next).tokenize()
    parser = Parser(scanner)
    root = parser.parse()
    instrumentTree(Node)
    env = Dict()
    eval(root, env, env)
    heappush = env['heappush']
    heappop = env['heappop']
    # Simple sanity test
    heap = []
    data = [1, 3, 5, 7, 9, 2, 4, 6, 8, 0]
    for item in data:
        heappush(heap, item)
    sort = []
    while heap:
        sort.append(heappop(heap))
    print sort
    unInstrumentTree(Node)
    checkoutput(2055134175)

if __name__ == '__main__':
    main()
