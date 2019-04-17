{
  getImei:{
    file: "b6.cpp",
    heap:{
      "ex_context": "*(ex_context)",
      "ex_cls": "im_22:11:ret",
      "ex_mid": "im_23:11:ret",
      "ex_fid": "im_18:20:ret",
      "ex_str": "im_20:29:ret",
      "ex_telephony": "im_21:25:ret",
      "ex_imei": "im_24:30:ret",
      "ret": "im_24:30:ret"
    },
    logs:[
      {
        ln: "15:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "16:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_15:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "18:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_15:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "20:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_15:18:ret",
          "im_arg2": "im_18:20:ret"
        }
      },
      {
        ln: "21:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_context)",
          "im_arg2": "im_16:21:ret",
          "im_arg3": "im_20:29:ret"
        }
      },
      {
        ln: "22:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "23:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_22:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "24:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_21:25:ret",
          "im_arg2": "im_23:11:ret"
        }
      }
    ]
  },
  Java_org_arguslab_native_1heap_1modify_MainActivity_heapModify:{
    file: "b6.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_mContext": "*(ex_mContext)",
      "ex_data": "*(ex_data)",
      "ex_deviceid": "im_24:30:ret",
      "ex_cd": "im_32:17:ret",
      "ex_fd": "im_33:19:ret"
    },
    logs:[
      {
        ln: "32:17",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_data)"
        }
      },
      {
        ln: "33:19",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_32:17:ret",
          "im_arg2": "'str'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "34:5",
        jfun: "_JNIEnv_SetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": "im_33:19:ret",
          "im_arg3": "im_24:30:ret"
        }
      }
    ]
  }
}
