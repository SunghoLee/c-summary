//
// Created by Xingwei Lin on 6/12/17.
//

#include <android/log.h>
#include <jni.h>
#include <stddef.h>
#include <assert.h>

#define LOG_TAG    "dynamic_register_multiple"
#define LOGI(...)  __android_log_print(ANDROID_LOG_INFO, LOG_TAG, __VA_ARGS__)
#define LOGE(...)  __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)
#define LOGD(...)  __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)

#define JNIREG_CLASS "org/arguslab/native_dynamic_register_multiple/MainActivity"

extern "C" {
JNIEXPORT void JNICALL native_send(JNIEnv *env, jobject thisObj, jstring data);
JNIEXPORT void JNICALL native_sendFoo(JNIEnv *env, jobject thisObj, jint index, jstring data);
JNIEXPORT void JNICALL native_sendBar(JNIEnv *env, jobject thisObj, jdouble dw, jstring data);
}

JNIEXPORT void JNICALL native_send(JNIEnv *env, jobject thisObj, jstring data) {
}

JNIEXPORT void JNICALL native_sendFoo(JNIEnv *env, jobject thisObj, jint index, jstring data) {
}

JNIEXPORT void JNICALL native_sendBar(JNIEnv *env, jobject thisObj, jdouble dw, jstring data) {
}

/**
* Table of methods associated with a single class.
*/
static JNINativeMethod gMethods[] = {
        {"send",    "(Ljava/lang/String;)V",  (void *) native_send},
        {"sendFoo", "(ILjava/lang/String;)V", (void *) native_sendFoo},
        {"sendBar", "(DLjava/lang/String;)V", (void *) native_sendBar},
};

/*
* Register several native methods for one class.
*/
static int registerNativeMethods(JNIEnv *env, const char *className,
                                 JNINativeMethod *locals, int numMethods) {
    jclass clazz = (jclass)env->FindClass(className);
    env->RegisterNatives(clazz, locals, numMethods);
    return JNI_TRUE;
}


/*
* Register native methods for all classes we know about.
*/
static int registerNatives(JNIEnv *env) {
    JNINativeMethod foolocs[] = {
            {"send",    "(Ljava/lang/String;)V",  (void *) native_send},
            {"sendFoo", "(ILjava/lang/String;)V", (void *) native_sendFoo},
            {"sendBar", "(DLjava/lang/String;)V", (void *) native_sendBar},
    };

    registerNativeMethods(env, JNIREG_CLASS, gMethods,
                               sizeof(gMethods) / sizeof(gMethods[0]));
        return JNI_FALSE;
}
