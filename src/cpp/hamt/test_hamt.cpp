// Copyright (C) 2017 Thomas Gilray, Kristopher Micinski
// See the notice in LICENSE.md


#include "gc.h"
#include "hamt.h"
#include <iostream>
#include <cstdlib>
#include <chrono>
#include <thread>


u64 utime()
{
    return ((u64)std::chrono::high_resolution_clock::now().time_since_epoch().count()) / 1000;
}


class tuple
{
public:
    const u64 x;
    const u64 y;
    const u64 z;

    tuple(u64 x, u64 y, u64 z) : x(x), y(y), z(z) {}

    u64 hash() const
    {
        const u8* data = reinterpret_cast<const u8*>(this);
        u64 h = 0xcbf29ce484222325;
        for (u32 i = 0; i < sizeof(tuple); ++i && ++data)
        {
            h = h ^ *data;
            h = h * 0x100000001b3;
        }

        return h;
    }

    bool operator==(const tuple& t) const
    {
        return t.x == this->x
            && t.y == this->y
            && t.z == this->z;
    }
};


void report_gc_size()
{
    // Can be added back in for debugging purposes if desired
    //std::cout << GC_get_heap_size() << std::endl;
}


void testround()
{
    const u32 offset = 1000+(std::rand() % 0x10000000);
    //std::cout << "Test round (offset=" << offset << ", threadid=" << std::this_thread::get_id() << "):" << std::endl;

    const hamt<tuple, tuple>* h = new ((hamt<tuple,tuple>*)GC_MALLOC(sizeof(hamt<tuple,tuple>))) hamt<tuple,tuple>();

    // *** This is the main value to scale the test up or down
    const u32 loops = 75000;
    // *************************

    // Add values
    for (u32 i = offset; i < offset+loops; ++i)
    {
        const tuple* const t = new ((tuple*)GC_MALLOC(sizeof(tuple))) tuple(i,i+1,i*i);
        h = h->insert(t,t);
        if (i % 50000 == 0) report_gc_size();
    }

    if (h->size() != loops) { std::cout << h->size() << std::endl; exit(1);}

    // Check values
    for (u32 j = 0; j < 2; ++j)
        for (u32 i = offset; i < offset+loops; ++i)
        {
            const tuple* const t = new ((tuple*)GC_MALLOC(sizeof(tuple))) tuple(i,i+1,i*i);
            const tuple* const t2 = h->get(t);
            if (t2 == 0 || !(*t == *t2))
                exit(1);
            if (i % 50000 == 0) report_gc_size();
        }

    // Check more values that don't exist
    for (u32 j = 0; j < 2; ++j)
        for (u32 i = 0x80000000; i < 0x80000000+loops; ++i)
        {
            const tuple* const t = new ((tuple*)GC_MALLOC(sizeof(tuple))) tuple(i,i+1,i*i);
            const tuple* const t2 = h->get(t);
            if (!(t2 == 0))
                exit(1);
            if (i % 50000 == 0) report_gc_size();
        }

    // Remove some values
    for (u32 j = 0; j < 3; ++j)
        for (u32 i = offset-100; i < offset+(loops/15)*j; ++i)
        {
            const tuple* const t = new ((tuple*)GC_MALLOC(sizeof(tuple))) tuple(i,i+1,i*i);
            h = h->remove(t);
            // Check that it is really gone
            const tuple* const t2 = h->get(t);
            if (!(t2 == 0))
                exit(1);
            if (i % 50000 == 0) report_gc_size();
        }

    if (h->size() != (loops - ((loops/15)*2))) { std::cout << h->size() << std::endl; exit(1);}
    h = 0;
}


int main()
{
    u32 rounds = 5;

    std::srand(12345);//utime());

    u64 best = 0xffffffffffffffff;
    u64 sum = 0;
    for (u32 i = 0; i < rounds; ++i)
    {
        u64 start = utime();
        testround();
        u64 end = utime();
        if ((end - start) < best)
            best = end - start;
        sum += (end - start);
    }

    std::cout << "Best timing: " << ((double)(best/1000)/1000.0) << "sec \t\t";
    std::cout << "Avg. timing: " << ((double)((sum/(rounds))/1000)/1000.0) << "sec" << std::endl;

    return 0;
}


