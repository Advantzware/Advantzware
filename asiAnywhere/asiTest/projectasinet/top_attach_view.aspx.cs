using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Diagnostics;

using System.IO;

public partial class attach_view : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {        
        //Session["spec_list_notes_title"] = Session["spec_list_notes_newtitle"];
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["User"] == null)
        {
            user_null_label.Text = "Session expired";
        }
        //Response.Write(Session["top_list_attach_rec_key"]);
        
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        try
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        }
        catch { }
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "top_attach_view.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            lblUser.Text = UserLogin.UserName;
            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");
            }
        }
        
        
    }
    protected void btn_add_new_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        btn_add_new.Visible = false;
    }
    protected void img_btn_list_attach_click(object sender, EventArgs e)
    {
        Response.Redirect("top_attach_list.aspx");
    }
    protected void img_btn_view_attach_click(object sender, EventArgs e)
    {
        Response.Redirect("top_attach_view.aspx");
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            
            if (FormView1.DataItemCount == 0)
            {
                btn_add_new.Visible = true;
            }
            if (FormView1.DataItemCount != 0)
            {
                btn_add_new.Visible = false;
            }
            
            
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
           
            
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox est = (TextBox)FormView1.FindControl("vEstimateTextBox");
            TextBox fgitem1 = (TextBox)FormView1.FindControl("vFGitemTextBox");
            est.Text = Convert.ToString(Session["order_entry_est_no"]);
            fgitem1.Text = Convert.ToString(Session["item"]);
            
            //UserClass UserLogin = (UserClass)Session["User"];
            Label date = (Label)FormView1.FindControl("vDateLabel");
            date.Text = DateTime.Now.ToShortDateString();
            
        }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox att = (TextBox)FormView1.FindControl("vAttFileTextBox");
        DropDownList open = (DropDownList)FormView1.FindControl("open_with_dropdownlist");
        TextBox est = (TextBox)FormView1.FindControl("vEstimateTextBox");
        TextBox item = (TextBox)FormView1.FindControl("vFGitemTextBox");
        FileUpload FileUpload1 = (FileUpload)FormView1.FindControl("FileUpload1");
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            if (FileUpload1.FileName != "")
            {
                string str1 = FileUpload1.PostedFile.FileName;
                string str2 = Path.GetFileName(str1);
                string str3 = Server.MapPath("uploadedimages");  //@"D:\webapps\uploadedimages\";  //give the path where you want to upload the file.

               string newpath = Server.MapPath("uploadedimages");
                try
                {
                    // Determine whether the directory exists.
                    if (Directory.Exists(newpath))
                    {
                    }
                    else
                    {
                        // Try to create the directory.
                        DirectoryInfo di = Directory.CreateDirectory(newpath);
                        string created = Directory.GetCreationTime(newpath).ToString();
                    }
                    
                }
                catch {  } 

                FileUpload1.PostedFile.SaveAs(Path.Combine(str3, str2));
                ObjectDataSource1.SelectParameters["prmAttFile"].DefaultValue = Path.Combine(str3, str2);
            }
            if (FileUpload1.FileName == "")
            {
                ObjectDataSource1.SelectParameters["prmAttFile"].DefaultValue = att.Text.Trim();
            }

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = est.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgitem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOpenWith"].DefaultValue = open.Text.Trim();
        }
        catch { }        

    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        //TextBox att = (TextBox)FormView1.FindControl("vAttFileTextBox");
        DropDownList open = (DropDownList)FormView1.FindControl("open_with_dropdownlist");
        TextBox est = (TextBox)FormView1.FindControl("vEstimateTextBox");
        TextBox item = (TextBox)FormView1.FindControl("vFGitemTextBox");
        FileUpload FileUpload1 = (FileUpload)FormView1.FindControl("FileUpload1");
        UserClass UserLogin = (UserClass)Session["User"];
        //try
        //{
            if (FileUpload1.FileName != "")
            {
                string str1 = FileUpload1.PostedFile.FileName;
                string str2 = Path.GetFileName(str1);
                string str3 = Server.MapPath("uploadedimages"); //@"D:\webapps\uploadedimages\";  //give the path where you want to upload the file.

                string newpath = Server.MapPath("uploadedimages");
                try
                {
                    // Determine whether the directory exists.
                    if (Directory.Exists(newpath))
                    {

                    }
                    else
                    {
                        // Try to create the directory.
                        DirectoryInfo di = Directory.CreateDirectory(newpath);
                        string created = Directory.GetCreationTime(newpath).ToString();
                    }

                }
                catch { } 

                FileUpload1.PostedFile.SaveAs(Path.Combine(str3, str2));
                ObjectDataSource1.SelectParameters["prmAttFile"].DefaultValue = Path.Combine(str3, str2);
            }
            if (FileUpload1.FileName == "")
            {
                //ObjectDataSource1.SelectParameters["prmAttFile"].DefaultValue = att.Text.Trim();
            }


            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


            ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = est.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgitem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOpenWith"].DefaultValue = open.Text.Trim();
        //}
        //catch { }
       
    }
    protected void btn_delete_click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = Convert.ToString(Session["order_entry_est_no"]);
            ObjectDataSource1.SelectParameters["prmFgitem"].DefaultValue = Convert.ToString(Session["order_entry_est_no"]);
        }
        catch { }
    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label reckey = (Label)FormView1.FindControl("vReckeyLabel");
            Label att = (Label)FormView1.FindControl("vAttFileLabel");
            Session["top_list_attach_rec_key"] = reckey.Text.Trim();
            
        }
        catch { }
    }

   
    protected void view_button_click(object sender, EventArgs e)
    {
        try
        {
            Label att = (Label)FormView1.FindControl("vAttFileLabel");

            string str1 = att.Text;

            string firstchar = str1.Substring(0, 1);
            string laststr = str1.Substring(1, str1.Length - 1);
            if (firstchar == "p" || firstchar == "P")
            {
                str1 = "D" + laststr;
            }

            string str2 = Path.GetFileName(str1);
            
            string path = str2;            
            if (path != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                {
                    
                    string fileName = str2;
                    System.IO.FileStream fs = null;
                    fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
                    byte[] btFile = new byte[fs.Length];
                    fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                    fs.Close();
                    Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                    Response.ContentType = "application/octet-stream";
                    Response.BinaryWrite(btFile);
                    Response.End();
                    fs = null;  
                }
            }
        }
        catch { }
    }
   
    
}
