#include <stdio.h>
#include <string.h>
#include "jni.h"

void Java_com_example_App_callJava(JNIEnv* env, jobject /*this*/, jobject app){ 
    jclass klass = env->GetObjectClass(app);
    jmethodID mid = env->GetMethodID(klass, "foo", "()V");
    env->CallVoidMethod(app, mid);
}
