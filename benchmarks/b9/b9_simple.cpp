//
// Created by Xingwei Lin on 6/12/17.
//

#include <android/log.h>
#include <jni.h>
#include <stddef.h>
#include <assert.h>

#define LOG_TAG    "leak_dynamic_register"
#define LOGI(...)  __android_log_print(ANDROID_LOG_INFO, LOG_TAG, __VA_ARGS__)
#define LOGE(...)  __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)
#define LOGD(...)  __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)

#define JNIREG_CLASS "org/arguslab/native_leak_dynamic_register/MainActivity"

int native_send(){
}
/**
* Table of methods associated with a single class.
*/
static JNINativeMethod gMethods[] = {
        {"send", "(Ljava/lang/String;)V", (void *) native_send},
};

/*
* Register several native methods for one class.
*/
static int registerNativeMethods(JNIEnv *env, const char *className,
                                 JNINativeMethod *gMethods, int numMethods) {
    jclass clazz;
    clazz = env->FindClass(className);
    if (clazz == NULL) {
        return JNI_FALSE;
    }
    if (env->RegisterNatives(clazz, gMethods, numMethods) < 0) {
        return JNI_FALSE;
    }
    
    int (*fun_ptr) ()= native_send;
    fun_ptr();

    return JNI_TRUE;
}


/*
* Register native methods for all classes we know about.
*/
static int registerNatives(JNIEnv *env) {
    if (!registerNativeMethods(env, JNIREG_CLASS, gMethods,
                               sizeof(gMethods) / sizeof(gMethods[0]))) {
        return JNI_FALSE;
    }

    return JNI_TRUE;
}
