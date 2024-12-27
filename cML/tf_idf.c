#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned long djb2(char* str) {
    unsigned long hash = 5381;
    int c;

    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;  // hash * 32 + c
    }
    return hash;
}


typedef struct HashNode {
    char* key;
    int int_value;
    float float_value;
    struct HashNode* next; 
} HashNode;


typedef struct HashMap {
    HashNode** nodes;
    int size; 
    int max_size_;
} HashMap;


HashMap* HashMap_create(int max_size) {
    HashNode** nodes = (HashNode**)calloc(max_size, sizeof(HashNode*));
    HashMap* map = (HashMap*)malloc(sizeof(HashMap));
    map->nodes = nodes;
    map->size = 0;
    map->max_size_ = max_size;
    return map;
}


void HashMap_insert(HashMap* map, char* key, int int_value, float float_value) {
    int idx = djb2(key) % map->max_size_;

    int i = 0;
    // Update value if current node already exists.
    HashNode** current = &map->nodes[idx];
    while (*current) {
        if (strcmp((*current)->key, key) == 0) {
            (*current)->int_value = int_value;
            (*current)->float_value = float_value;
            return;
        }
        i += 1;
        current = &((*current)->next);
    }

    // Node was not found, insert a new one.
    HashNode* node = (HashNode*) malloc(sizeof(HashNode));
    node->key = malloc(strlen(key) + 1);  // +1 to account for '\0'.
    strcpy(node->key, key);
    node->int_value = int_value;
    node->float_value = float_value;
    node->next = NULL;
    *current = node;
    map->size += 1;
}


int HashMap_get_int(HashMap* map, char* key) {
    int idx = djb2(key) % map->max_size_;

    HashNode* current = map->nodes[idx];
    while (current != NULL) {
        if (strcmp(current->key, key) == 0) {
            return current->int_value;
        }
        current = current->next; 
    }
    return -1; 
}


float HashMap_get_float(HashMap* map, char* key) {
    int idx = djb2(key) % map->max_size_;

    HashNode* current = map->nodes[idx];
    while (current != NULL) {
        if (strcmp(current->key, key) == 0) {
            return current->float_value;
        }
        current = current->next; 
    }
    return -1.0; 
}


typedef struct {
    HashMap* map;
    int idx;
    HashNode* next;

} HashMapIterator;


HashMapIterator* HashMapIterator_create(HashMap* map) {
    HashMapIterator* iter = (HashMapIterator*)malloc(sizeof(HashMapIterator));
    iter->map = map;
    iter->idx = -1;
    iter->next = NULL;
    return iter;
}

void HashMapIterator_reset(HashMapIterator* iter, HashMap* map) {
    iter->map = map;
    iter->idx = -1;
    iter->next = NULL;
}

HashNode* HashMapIterator_next(HashMapIterator* iter) {
    // First try to get the next item in the hash bucket.
    if (iter->next && iter->next->next) {
        iter->next = iter->next->next;
        return iter->next;
    }

    // If no such item exists, keep iterating over the hash buckets
    // to find the next item.
    iter->idx += 1;
    while (iter->idx <= iter->map->max_size_ && !iter->map->nodes[iter->idx]) {
        iter->idx += 1;
    }

    // If we went outside the HashMap, this means there were no more items
    // and we just return NULL.
    if (iter->idx >= iter->map->max_size_) {
        return NULL;
    }

    // Otherwise return the next item we found.
    iter->next = iter->map->nodes[iter->idx];
    return iter->next;
}


// TODO(eugen): This function does a malloc/free in each call. Consider
// passing in a HashMapIterator instead.
float dot(HashMap* left, HashMap* right) {
    float result = 0.0;
    HashMapIterator* iter = HashMapIterator_create(left);
    HashNode* node = HashMapIterator_next(iter);
    while (node) {
        float right_val = HashMap_get_float(right, node->key);
        if (right_val > 0) {
            result += node->float_value * right_val;
        }
        node = HashMapIterator_next(iter);
    }
    free(iter);
    return result;
}


typedef struct {
    int idx;
    float score;
} Score;


int score_cmp(const void* left, const void* right) {
    return -(int)(((Score*)left)->score - ((Score*)right)->score);
}


// Scores the query against all documents.
// score_buffer is expected to be an array of Scores with size n_documents.
// When the function returns, Scores will be sorted by dot(query, document) score.
void score_and_sort(
    HashMap* query, 
    HashMap* documents[], 
    int n_documents, 
    Score* score_buffer) {
    for (int i = 0; i < n_documents; i++) {
        score_buffer[i].idx = i; 
        score_buffer[i].score = dot(query, documents[i]);
    }
    qsort(score_buffer, n_documents, sizeof(Score), score_cmp);
}


int main(void) {
    FILE* file = fopen("data/tiny_shakespear.txt", "r");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    // Create vocabulary.
    HashMap* vocabulary = HashMap_create(32768);
    int buffer_size = 1024;
    int n_chunks = 0;
    char buffer[buffer_size];
    while (fgets(buffer, buffer_size, file)) {
        // Remove blank lines.
        if (buffer[0] == '\n' || buffer[0] == '\r') {
            continue;
        }

        n_chunks += 1;
        char* token; 
        token = strtok(buffer, " \n\r");
        while (token != NULL) {
            if (HashMap_get_int(vocabulary, token) < 0) {
                HashMap_insert(vocabulary, token, vocabulary->size, -1.0);
            }
            token = strtok(NULL, " \n\r");
        }
    }
    printf("Vocabulary: %d, chunks: %d\n", vocabulary->size, n_chunks);


    // TODO(eugen): This part of the code duplicates the file reading above
    // and has lead to some bugs when we change one but not the other. 
    // Chunk and embed text. 
    // For now, each line is a separate chunk.
    rewind(file);
    HashMap* vectors[n_chunks];
    int i = 0;
    while (fgets(buffer, buffer_size, file)) {
        // Remove blank lines.
        if (buffer[0] == '\n' || buffer[0] == '\r') {
            continue;
        }

        HashMap* vec = HashMap_create(32);
        char* token; 
        token = strtok(buffer, " \n\r");
        while (token) {
            int idx = HashMap_get_int(vocabulary, token);
            if (idx >= 0) {
                float value = HashMap_get_float(vec, token);
                HashMap_insert(vec, token, idx, 1.0 ? value < 0 : value + 1.0);
            }
            token = strtok(NULL, " \n\r");
        }

        vectors[i] = vec;
        i += 1;
    }
    fclose(file);


    // This iterator will be reused in all the code below. Use a dummy
    // HashMap to initialize with for now. 
    HashMapIterator* iter = HashMapIterator_create(vocabulary);  

    // Compute term frequencies. 
    for (int i = 0; i < n_chunks; i++) {
        HashMapIterator_reset(iter, vectors[i]);
        HashNode* node = HashMapIterator_next(iter);
        float total = 0.0;
        while (node) {
            total += node->float_value;
            node = HashMapIterator_next(iter);
        }

        HashMapIterator_reset(iter, vectors[i]);
        node = HashMapIterator_next(iter);
        while (node) {
            node->float_value /= total;
            node = HashMapIterator_next(iter);
        }
    }

    // Compute inverse document frequencies.
    HashMap* idf = HashMap_create(vocabulary->size);
    for (int i = 0; i < n_chunks; i++) {
        HashMapIterator_reset(iter, vectors[i]);
        HashNode* node = HashMapIterator_next(iter);
        while (node) {
            int curr_df = HashMap_get_int(idf, node->key);
            curr_df = 1 ? curr_df < 0 : curr_df + 1;
            float curr_idf = log(n_chunks / curr_df);
            HashMap_insert(idf, node->key, curr_df, curr_idf);
            node = HashMapIterator_next(iter);
        }
    }

    // Update vectors with tf-idf score.
    for (int i = 0; i < n_chunks; i++) {
        HashMapIterator_reset(iter, vectors[i]);
        HashNode* node = HashMapIterator_next(iter);
        while (node) {
            float idf_score = HashMap_get_float(idf, node->key);
            if (idf_score < 0.0) {
                idf_score = log(n_chunks);
            }
            node->float_value = node->float_value * idf_score;
            node = HashMapIterator_next(iter);
        }
    }

    // Print top k;
    Score scores[n_chunks];
    int idx = 128, k = 10;
    score_and_sort(vectors[idx], vectors, n_chunks, scores);

    printf("Query vector (%d): ", idx);
    HashMapIterator_reset(iter, vectors[idx]);
    HashNode* node = HashMapIterator_next(iter);
    while (node) {
        printf("%s ", node->key);
        node = HashMapIterator_next(iter);
    }   
    printf("\n\n");

    printf("Top matches:\n");
    for (int i = 0; i < k; i++) {
        printf("(%5d, %5.2f) -> ", scores[i].idx, scores[i].score);
        HashMapIterator_reset(iter, vectors[scores[i].idx]);
        HashNode* node = HashMapIterator_next(iter);
        while (node) {
            printf("%s ", node->key);
            node = HashMapIterator_next(iter);
        }
        printf("\n");
    }

	return 0;
}