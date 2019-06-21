
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

public partial class check_invoice : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {
        try
        {
            GridView1_SelectedIndex(sender, e);
        }
        catch { }
    }

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "check_invoice.aspx";
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
            lblUser.Text = UserLogin.UserName;
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
            if (Session["check_invoice_index"] == null)
            {
               

                Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_attach_invlist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_attach_invlist_inv"] = GridView1.SelectedRow.Cells[3].Text;
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["check_invoice_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["check_invoice_index"]);

                Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_attach_invlist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_attach_invlist_inv"] = GridView1.SelectedRow.Cells[3].Text;
                
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
        endchk_TextBox.Text = "99999999";


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
        Session["check_invoice_index"] = GridView1.SelectedIndex;

        Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["top_attach_invlist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["top_attach_invlist_inv"] = GridView1.SelectedRow.Cells[3].Text;
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
               
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmbegcchk"].DefaultValue = begchk_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendchk"].DefaultValue = endchk_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbeginv"].DefaultValue = beginv_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendinv"].DefaultValue = endinv_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvendname"].DefaultValue = vendname_TextBox.Text.Trim();

        Session["check_invoice_index"] = null;
        
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        vendor_TextBox.Text = "";
        Session["check_invoice_vendor"] = vendor_TextBox.Text;
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor_TextBox.Text.Trim();
        Session["check_invoice_index"] = null;
        
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

         ObjectDataSource3.SelectParameters["prmchkbal"].DefaultValue = "chkbal";
         ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
         ObjectDataSource3.SelectParameters["prmbegcchk"].DefaultValue = begchk_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmendchk"].DefaultValue = endchk_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmbeginv"].DefaultValue = beginv_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmendinv"].DefaultValue = endinv_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmbegdate"].DefaultValue = begdtTextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmenddate"].DefaultValue = enddtTextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmvend"].DefaultValue = vendor_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmvendname"].DefaultValue = vendname_TextBox.Text.Trim();

         SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
         try
         {
             conn.Open();

             string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'check_invoice.aspx' ";
             SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
             DataSet ds = new DataSet();
             da.Fill(ds);

             if (ds.Tables[0].Rows.Count == 0)
             {
                 SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8 ) values ('" + UserLogin.UserName + "','check_invoice.aspx','" + vendor_TextBox.Text.Trim() + "','" + vendname_TextBox.Text.Trim() + "','" + beginv_TextBox.Text.Trim() + "','" + endinv_TextBox.Text.Trim() + "', '" + begchk_TextBox.Text.Trim() + "','" + endchk_TextBox.Text.Trim() + "', '" + begdtTextBox.Text.Trim() + "','" + enddtTextBox.Text.Trim() + "')", conn);
                 cmd_insert.ExecuteNonQuery();
             }
             else
             {
                 SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + vendor_TextBox.Text.Trim() + "', field2 = '" + vendname_TextBox.Text.Trim() + "', field3 = '" + beginv_TextBox.Text.Trim() + "', field4 = '" + endinv_TextBox.Text.Trim() + "', field5 = '" + begchk_TextBox.Text.Trim() + "', field6 = '" + endchk_TextBox.Text.Trim() + "', field7 = '" + begdtTextBox.Text.Trim() + "', field8 = '" + enddtTextBox.Text.Trim() + "'  where user_name = '" + UserLogin.UserName + "' and prog_name ='check_invoice.aspx' ", conn);
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



         Label vpath = (Label)FormView2.FindControl("chkbalLabel");

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
     protected void GridView1_Unload(object sender, EventArgs e)
     {
     }


    
    

}
