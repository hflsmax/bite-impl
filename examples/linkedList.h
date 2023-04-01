#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    void* data;
    struct Node* next;
} Node;

typedef struct LinkedList {
    Node* head;
    Node* tail;
} LinkedList;

typedef struct Iterator {
    Node* current;
    LinkedList* list;
} Iterator;

// Function prototypes
LinkedList* ListNew();
LinkedList* ListInit(LinkedList* list, int n);
void ListAppend(LinkedList* list, void* data);
void* ListPopFirstElement(LinkedList* list);
void ListRemoveFirstElement(LinkedList* list);
Iterator* ListGetIter(LinkedList* list);
bool IterHasNext(Iterator* iter);
void* IterNext(Iterator* iter);
void IterSet(Iterator* iter, void* data);
void* IterGet(Iterator* iter);
void IterSetInt(Iterator* iter, int data);
int IterGetInt(Iterator* iter);
void IterRemoveNext(Iterator* iter);

// Function implementations
#define ListNewStatic() (&((LinkedList){.head = NULL, .tail = NULL}))

LinkedList* ListNew() {
    LinkedList* list = (LinkedList*)malloc(sizeof(LinkedList));
    list->head = NULL;
    list->tail = NULL;
    return list;
}

LinkedList* ListInit(LinkedList* list, int n) {
    list->head = NULL;
    list->tail = NULL;

    for (int i = 0; i < n; ++i) {
        ListAppend(list, (void*)i);
    }
    return list;
}

void ListAppend(LinkedList* list, void* data) {
    Node* newNode = (Node*)malloc(sizeof(Node));

    newNode->data = data;
    newNode->next = NULL;

    if (list->head == NULL) {
        list->head = newNode;
        list->tail = newNode;
    } else {
        list->tail->next = newNode;
        list->tail = newNode;
    }
}

void* ListPopFirstElement(LinkedList* list) {
    Node* nodeToRemove = list->head;
    void* data = nodeToRemove->data;
    list->head = nodeToRemove->next;
    if (list->head == NULL) {
        list->tail = NULL;
    }

    free(nodeToRemove);
    return data;
}

void ListRemoveFirstElement(LinkedList* list) {
    Node* nodeToRemove = list->head;
    list->head = nodeToRemove->next;
    if (list->head == NULL) {
        list->tail = NULL;
    }

    free(nodeToRemove);
}

Iterator* ListGetIter(LinkedList* list) {
    Iterator* iter = (Iterator*)malloc(sizeof(Iterator));
    iter->current = list->head;
    iter->list = list;
    return iter;
}

bool IterHasNext(Iterator* iter) {
    return iter->current->next != NULL;
}

void* IterNext(Iterator* iter) {
    iter->current = iter->current->next;
    return iter->current->data;
}

void IterSet(Iterator* iter, void* data) {
    iter->current->data = data;
}

void* IterGet(Iterator* iter) {
    return iter->current->data;
}

void IterSetInt(Iterator* iter, int data) {
    iter->current->data = data;
}

int IterGetInt(Iterator* iter) {
    return iter->current->data;
}

void IterRemoveNext(Iterator* iter) {
    Node* nodeToRemove = iter->current->next;
    iter->current->next = nodeToRemove->next;
    if (nodeToRemove == iter->list->tail) {
        iter->list->tail = iter->current;
    }
    free(nodeToRemove);
}
