
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class apinv_bal : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "disbur_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        


        try
        {
            if (Session["apinv_bal_index"] == null)
            {
                
                Session["top_attach_invlist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("templabel")).Text;
                Session["top_attach_invlist_inv"] = GridView1.SelectedRow.Cells[1].Text;
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["apinv_bal_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["apinv_bal_index"]);
                Session["top_attach_invlist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("templabel")).Text;
                Session["top_attach_invlist_inv"] = GridView1.SelectedRow.Cells[1].Text; 
                
            }
        }
        catch
        {
           
        }
        Session["Rowuser"] = UserLogin.UserName;

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            
        }
        endinv_TextBox.Text = "zzzzzzzzzzzz";
        TextBox1.Text = "9999";


    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }


    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["apinv_bal_index"] = GridView1.SelectedIndex;
        Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["top_attach_invlist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("templabel")).Text;
        Session["top_attach_invlist_inv"] = GridView1.SelectedRow.Cells[1].Text;
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
               
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmupto"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbeginv"].DefaultValue = beginv_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendinv"].DefaultValue = endinv_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvendname"].DefaultValue = vendname_TextBox.Text.Trim();
        if(CheckBox1.Checked)
             ObjectDataSource1.SelectParameters["prmvopen"].DefaultValue = "Yes";
        else
            ObjectDataSource1.SelectParameters["prmvopen"].DefaultValue = "No";
        Session["apinv_bal_index"] = null;
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        vendor_TextBox.Text = "";
        //checkno_TextBox.Text = "";


        Session["disbur_list_vendor"] = vendor_TextBox.Text;
        //Session["disbur_list_checkno"] = checkno_TextBox.Text.Trim();
        

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor_TextBox.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmcheckno"].DefaultValue = checkno_TextBox.Text.Trim();

        Session["apinv_bal_index"] = null;
    }
    

     protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

     protected void print_Click(object sender, EventArgs e)
     {
         UserClass UserLogin = (UserClass)Session["User"];

         ObjectDataSource3.SelectParameters["prminvbal"].DefaultValue = "invbal";
         ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
         ObjectDataSource3.SelectParameters["prmupto"].DefaultValue = TextBox1.Text.Trim();
         ObjectDataSource3.SelectParameters["prmbeginv"].DefaultValue = beginv_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmendinv"].DefaultValue = endinv_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmvend"].DefaultValue = vendor_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmvendname"].DefaultValue = vendname_TextBox.Text.Trim();
         if (CheckBox1.Checked)
             ObjectDataSource3.SelectParameters["prmvopen"].DefaultValue = "Yes";
         else
             ObjectDataSource3.SelectParameters["prmvopen"].DefaultValue = "No";



         SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
         try
         {
             conn.Open();

             string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'apinv_bal.aspx' ";
             SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
             DataSet ds = new DataSet();
             da.Fill(ds);

             if (ds.Tables[0].Rows.Count == 0)
             {
                 SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, chk_field1) values ('" + UserLogin.UserName + "','apinv_bal.aspx','" + vendor_TextBox.Text.Trim() + "','" + vendname_TextBox.Text.Trim() + "','" + beginv_TextBox.Text.Trim() + "','" + endinv_TextBox.Text.Trim() + "','" + TextBox1.Text.Trim() + "','" + Convert.ToString(CheckBox1.Checked) + "')", conn);
                 cmd_insert.ExecuteNonQuery();
             }
             else
             {
                 SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + vendor_TextBox.Text.Trim() + "', field2 = '" + vendname_TextBox.Text.Trim() + "', field3 = '" + beginv_TextBox.Text.Trim() + "', field4 = '" + endinv_TextBox.Text.Trim() + "', field5 = '" + TextBox1.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='apinv_bal.aspx' ", conn);
                 cmd_update.ExecuteNonQuery();
             }

         }
         catch 
         {             
             conn.Close();
         }
         finally
         {
             conn.Close();
         }



         Label vpath = (Label)FormView2.FindControl("invbalLabel");
        
         try
         {
             if (vpath.Text != "")
             {
                 string path = vpath.Text;
                 string path2 = @"/pdfs/" + path;
                 Session["open_tranorder_list"] = path2;
                 if (path2 != "")
                 {
                     if (!Request.Browser.Browser.Contains("Safari"))
                         Response.Write("<script>window.open('print_tranorder_list.aspx'); target='_blank'</script>");

                 }
             }
             

         }
         catch { }
     }

     protected void FormView2_PreRender(object sender, EventArgs e)
     {
         
     }

    
    

}
