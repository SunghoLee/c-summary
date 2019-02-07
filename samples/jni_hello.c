#include <stdio.h>
#include <string.h>
#include "jni.h"

int what() {
       char src[40];
      char dest[100];
        
         memset(dest, '\0', sizeof(dest));
            strcpy(src, "This is tutorialspoint.com");
               strcpy(dest, src);

                  printf("Final copied string : %s\n", dest);
                     
                     return(0);
}

void Java_com_example_App_callJava(JNIEnv* env, jobject /*this*/, jobject app){ 
    jclass klass = env->GetObjectClass(app);
    jmethodID mid = env->GetMethodID(klass, "foo", "()V");
    env->CallVoidMethod(app, mid);
}
