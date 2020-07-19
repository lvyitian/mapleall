/*
 * Copyright (c) [2020] Huawei Technologies Co., Ltd. All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan Permissive Software License v2.
 * You can use this software according to the terms and conditions of the MulanPSL - 2.0.
 * You may obtain a copy of MulanPSL - 2.0 at:
 *
 *   https://opensource.org/licenses/MulanPSL-2.0
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the MulanPSL - 2.0 for more details.
 */

#include "../include/mempool.h"
#include "../include/mempool_allocator.h"
#include "../include/maple_string.h"
#include "mpl_logging.h"
#include <iostream>
#include "securec.h"

using namespace maple;

struct structure {
  int id;
  float height;
};
using Structure = struct structure;

class MyClass {
 public:
  MyClass(int id, const char *name) {
    _id = id;
    _name = name;
  };
  ~MyClass(){};
  int _id;
  std::string _name;
};

int main() {
  using namespace std;
  // 1. Create a memory pool controler instance;
  MemPoolCtrler mpc;

  // 2. Create two memory pools on mpc
  MemPool *mp1 = mpc.NewMemPool("Test Memory Pool 1");
  MemPool *mp2 = mpc.NewMemPool("Test Memory Pool 2");

  // 3. Usage of memory pool, Malloc/Call on primitive types
  // char string
  const int lengthofHelloWorld = 12;
  char *charP = static_cast<char *>(mp1->Malloc(lengthofHelloWorld * sizeof(char)));
  errno_t cpyRes = strcpy_s(charP, lengthofHelloWorld, "Hello world");
  if (cpyRes != 0){
    std::cout << "call strcpy_s failed" << std::endl;
    return 0;
  }
  std::cout << charP << std::endl;

  // int, float, double
  int *intP = static_cast<int *>(mp1->Calloc(10 * sizeof(int)));
  ASSERT(intP, "null ptr check  ");
  for (int i = 0; i < 10; i++) {
    intP[i] = i * i;
  }

  for (int i = 0; i < 9; i++) {
    std::cout << intP[i] << " ,";
  }
  std::cout << intP[9] << std::endl;

  float *floatP = static_cast<float *>(mp1->Malloc(10 * sizeof(float)));
  for (int i = 0; i < 10; i++) {
    floatP[i] = 10.0 / (i + 1);
  }

  for (int i = 0; i < 9; i++) {
    std::cout << floatP[i] << " ,";
  }
  std::cout << floatP[9] << std::endl;

  // 4. Allocate memory on struct
  Structure *structP = mp1->New<Structure>();

  structP->height = 1024;
  structP->id = 0;
  std::cout << "Structure: my_struct height=" << structP->height << " feet,"
            << "id=    x" << structP->id << std::endl;
  // 5. Allocate memory on class constructor
  MyClass *myClass = mp1->New<MyClass>(1, "class name");

  std::cout << "Class: my_class id=" << myClass->_id << " , name=" << myClass->_name << std::endl;
  // 6. MP push
  mp2->Push();

  // 7. Memory Pool supports std library such list, vector, string, map, set.
  // using mempool
  MapleAllocator mpAllocator(mp2);

  // vector
  MapleVector<int> myVector(mpAllocator.Adapter());

  for (int i = 0; i < 10; i++) {
    myVector.push_back(i * i * i);
  }
  MapleVector<int>::iterator itr;
  for (itr = myVector.begin(); itr != myVector.end(); itr++) {
    std::cout << *itr << " ,";
  }
  std::cout << std::endl;

  // stack
  MapleQueue<int> myQueue(mpAllocator.Adapter());
  myQueue.push_back(1);
  myQueue.push_back(2);
  myQueue.push_back(3);

  std::cout << "Queue front is" << myQueue.front() << std::endl;
  std::cout << "Queue back is" << myQueue.back() << std::endl;
  myQueue.pop_back();

  std::cout << "after pop() vector top is" << myQueue.back() << std::endl;

  // String
  MapleString myString(mp2);
  myString = "Using my mempool";
  myString += " example";

  std::cout << myString << std::endl;

  // list
  MapleList<float> myList(mpAllocator.Adapter());
  for (int i = 0; i < 10; i++) {
    myList.push_back(1000.0 / (i + 1));
  }
  MapleList<float>::iterator listItr;
  for (listItr = myList.begin(); listItr != myList.end(); listItr++) {
    std::cout << *listItr << " ,";
  }
  std::cout << std::endl;

  // Map
  MapleMap<int, MapleString> myMap(std::less<int>(), mpAllocator.Adapter());
  for (int i = 0; i < 10; i++) {
    MapleString temp(mp2);
    temp += std::to_string(i);
    temp += " value";
    myMap.insert(std::pair<int, MapleString>(i, temp));
  }

  MapleMap<int, MapleString>::iterator mapItr;
  for (mapItr = myMap.begin(); mapItr != myMap.end(); mapItr++) {
    std::cout << "key= " << mapItr->first << ", value=" << mapItr->second << std::endl;
  }

  // Set
  MapleSet<MapleString> mySet(std::less<MapleString>(), mpAllocator.Adapter());
  for (int i = 0; i < 10; i++) {
    MapleString temp(mp2);
    temp += std::to_string(i * i);
    temp.append(" set values");
    mySet.insert(temp);
  }

  MapleSet<MapleString>::iterator setItr;
  for (setItr = mySet.begin(); setItr != mySet.end(); setItr++) {
    std::cout << "set value =" << *setItr << std::endl;
  }

  // MP Pop
  mp2->Pop();

  // Delete memory pool
  mpc.DeleteMemPool(mp1);
  mpc.DeleteMemPool(mp2);
  return 1;
}
