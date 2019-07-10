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
using System.Data.SqlClient;
using System.Linq;

public partial class MasterPagestimate : System.Web.UI.MasterPage
{
    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();


        if (fname == "order_estimate_aspx")
        {
            librowseorder.Attributes.Add("class", "selected");
        }
        else if (fname == "view_order_estimate_aspx")
        {
            livieworder.Attributes.Add("class", "selected");
        }
        else if (fname == "viewitem_aspx" || fname == "view_item_estimate_aspx")
        {
            liviewitem.Attributes.Add("class", "selected");
        }
        else if (fname == "mischarge_aspx")
        {
            limischgs.Attributes.Add("class", "selected");
        }
        else if (fname == "f")
        {
            lijobprob.Attributes.Add("class", "selected");
        }
        else if (fname == "releases_aspx")
        {
            lirelease.Attributes.Add("class", "selected");
        }
        else if (fname == "ordertotal_aspx")
        {
            liordertot.Attributes.Add("class", "selected");
        }
        else if (fname == "df")
        {
            lifgitem.Attributes.Add("class", "selected");
        }
        else if (fname == "ff")
        {
            liinvoices.Attributes.Add("class", "selected");
        }
        else
        {
            lishipnotes.Attributes.Add("class", "selected");
        }

        Session["item_list_item"] = Session["item"];
        Session["add_order_notes"] = null;
        Session["show"] = null;
        string swhere = Request.Form["radio1"];

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;



                string vUserId = UserLogin.UserName;
                string vPage = "order_estimate.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;




                func1 f1 = new func1();
                //Response.Write(Page);

                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    lnk_brwsorder.Visible = true;
                    //brwsorder.Visible = true;

                }

                if (vCanRun == false)
                {
                    lnk_brwsorder.Visible = false;
                    //brwsorder.Visible = false;

                }

                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    string cmd = "select * from button_maintain where parent = 'order_estimate.aspx' ";
                    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    if (ds.Tables[0].Rows.Count == 0)
                    {
                        SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,btn10,chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10,user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,user11,user12,user13,user14,user15) values ('order_estimate.aspx','Order Entry & Order Status','View Order','List Item','View Item','Misc Chgs','Job Status','Releases','Order Total','Item Status','Invoices','Ship Notes','True','True','True','True','True','True','True','True','True','True','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "')", conn);
                        cmd_insert.ExecuteNonQuery();
                    }

                    foreach (DataRow dr in ds.Tables[0].Rows)
                    {
                        string[] ss1 = dr["user1"].ToString().Split(',');
                        string[] ss2 = dr["user2"].ToString().Split(',');
                        string[] ss3 = dr["user3"].ToString().Split(',');
                        string[] ss4 = dr["user4"].ToString().Split(',');
                        string[] ss5 = dr["user5"].ToString().Split(',');
                        string[] ss6 = dr["user6"].ToString().Split(',');
                        string[] ss7 = dr["user7"].ToString().Split(',');
                        string[] ss8 = dr["user8"].ToString().Split(',');
                        string[] ss9 = dr["user9"].ToString().Split(',');
                        string[] ss10 = dr["user10"].ToString().Split(',');
                        if (ss1.Contains(UserLogin.UserName) || ss1.Contains("*"))
                        {
                            //vieworder.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                            if (dr["chk1"].ToString() == "False")
                            livieworder.Attributes.Add("style", "display:none");
                        }
                        if (ss2.Contains(UserLogin.UserName) || ss2.Contains("*"))
                        {
                           // listitem.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                            if (dr["chk2"].ToString() == "False")
                            lilistitem.Attributes.Add("style", "display:none");
                        }
                        if (ss3.Contains(UserLogin.UserName) || ss3.Contains("*"))
                        {
                            //viewitem.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                            if (dr["chk3"].ToString() == "False")
                            liviewitem.Attributes.Add("style", "display:none");
                        }
                        if (ss4.Contains(UserLogin.UserName) || ss4.Contains("*"))
                        {
                           // misccharge.Visible = Convert.ToBoolean(dr["chk4"].ToString());
                            if (dr["chk4"].ToString() == "False")
                            limischgs.Attributes.Add("style", "display:none");
                        }
                        if (ss5.Contains(UserLogin.UserName) || ss5.Contains("*"))
                        {
                            //jobprod.Visible = Convert.ToBoolean(dr["chk5"].ToString());
                            if (dr["chk5"].ToString() == "False")
                            lijobprob.Attributes.Add("style", "display:none");
                        }
                        if (ss6.Contains(UserLogin.UserName) || ss6.Contains("*"))
                        {
                           // releases.Visible = Convert.ToBoolean(dr["chk6"].ToString());
                            if (dr["chk6"].ToString() == "False")
                            lirelease.Attributes.Add("style", "display:none");
                        }
                        if (ss7.Contains(UserLogin.UserName) || ss7.Contains("*"))
                        {
                            //ordertotal.Visible = Convert.ToBoolean(dr["chk7"].ToString());
                            if (dr["chk7"].ToString() == "False")
                            liordertot.Attributes.Add("style", "display:none");
                        }
                        if (ss8.Contains(UserLogin.UserName) || ss8.Contains("*"))
                        {
                            //fgitem.Visible = Convert.ToBoolean(dr["chk8"].ToString());
                            if (dr["chk8"].ToString() == "False")
                            lifgitem.Attributes.Add("style", "display:none");
                        }
                        if (ss9.Contains(UserLogin.UserName) || ss9.Contains("*"))
                        {
                           // invoice.Visible = Convert.ToBoolean(dr["chk9"].ToString());
                            if (dr["chk9"].ToString() == "False")
                            liinvoices.Attributes.Add("style", "display:none");
                        }
                        if (ss10.Contains(UserLogin.UserName) || ss10.Contains("*"))
                        {
                           //shipnotes.Visible = Convert.ToBoolean(dr["chk10"].ToString());
                            if (dr["chk10"].ToString() == "False")
                            lishipnotes.Attributes.Add("style", "display:none");
                        }
                                                
                        
                    }
                    conn.Close();
                }
                catch { conn.Close(); }
            


                string vUserIdvieworder = UserLogin.UserName;
                string vPagevieworder = "view_order_estimate.aspx";
                string aUsersvieworder = null;
                string PrmCompvieworder = null;
                bool vCanCreatevieworder = false;
                bool vCanRunvieworder = false;
                bool vCanUpdatevieworder = false;
                bool vCanDeletevieworder = false;

                func1 f1vieworder = new func1();
                //Response.Write(Page);
                f1vieworder.CheckProgramPermissions(vPagevieworder, vUserIdvieworder, ref  vCanCreatevieworder, ref  vCanRunvieworder, ref  vCanUpdatevieworder, ref  vCanDeletevieworder, ref  PrmCompvieworder, ref  aUsersvieworder);

                lblComp.Text = PrmCompvieworder;
                
               
            }
        }
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    protected void lnk_vieworder_Click(object sender, EventArgs e)
    {

        if (Session["order_est"] != null)
        {
            Session["order_est"] = Session["order_est"];
            Response.Redirect("view_order_estimate.aspx");
        }
        else
        {
            Response.Write("<script> alert('Please Select a Record')</script>");
        }
    }



    protected void lnk_listitem_Click(object sender, EventArgs e)
    {
        if (Session["order_est"] != null)
        {
            Session["viewitem"] = Session["order_est"];

            Response.Redirect("viewItem.aspx");
        }
        else
        {
            Response.Write("<script> alert('Please Select a Record')</script>");
        }

    }


    protected void lnk_viewitem_Click(object sender, EventArgs e)
    {
        if (Session["view_line_est"] != null)
        {
            Response.Redirect("view_item_estimate.aspx");
        }
        else
        {
            Response.Write("<script> alert('Please Select a Record')</script>");
        }


    }



    protected void lnk_mischgs_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["charge"] = Session["order"];
            Response.Redirect("mischarge.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }
    }
    protected void lnk_jobprod_Click(object sender, EventArgs e)
    {

        if (Session["order"] != null)
        {
            //Session["jobprod"] = Session["order_entry"];
            Session["Material"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("jobprod.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }
    }
    protected void lnk_releases_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["releases"] = Session["order"];
            Response.Redirect("releases.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }
    }
    protected void lnk_ordertotal_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["ordertotal"] = Session["order"];
            Response.Redirect("ordertotal.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }
    }
    protected void lnk_fgitem_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["fgitem"] = Session["order"];
            Response.Redirect("fgitem.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }
    }
    protected void lnk_shipnotes_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["shipnotes"] = Session["order"];
            Response.Redirect("shipnotes.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }
    }
    protected void lnk_invoices_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["brwsinvoice"] = Session["order"];
            Response.Redirect("browsinvoice.aspx");
        }
        else
        {
            Response.Write("<script>alert('Please Select a Record')</script>");
        }

    }





    protected void lnk_brwsorder_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
    protected void brwsorder_Click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
    protected void vieworder_Click(object sender, ImageClickEventArgs e)
    {
        lnk_vieworder_Click(sender, e);
    }
    protected void listitem_Click(object sender, ImageClickEventArgs e)
    {
        lnk_listitem_Click(sender, e);
    }
    protected void viewitem_Click(object sender, ImageClickEventArgs e)
    {
        lnk_viewitem_Click(sender, e);
    }



    protected void misccharge_Click(object sender, ImageClickEventArgs e)
    {
        lnk_mischgs_Click(sender, e);
    }
    protected void jobprod_Click(object sender, ImageClickEventArgs e)
    {
        lnk_jobprod_Click(sender, e);
    }
    protected void releases_Click(object sender, ImageClickEventArgs e)
    {
        lnk_releases_Click(sender, e);
    }
    protected void ordertotal_Click(object sender, ImageClickEventArgs e)
    {
        lnk_ordertotal_Click(sender, e);
    }
    protected void fgitem_Click(object sender, ImageClickEventArgs e)
    {
        lnk_fgitem_Click(sender, e);
    }
    protected void shipnotes_Click(object sender, ImageClickEventArgs e)
    {
        lnk_shipnotes_Click(sender, e);
    }
    protected void invoice_Click(object sender, ImageClickEventArgs e)
    {
        lnk_invoices_Click(sender, e);
    }

    protected void img_btn_add_click(object sender, ImageClickEventArgs e)
    {
        Session["add_buton"] = "add";
        Response.Redirect("view_order_estimate.aspx");

    }
    //protected void img_btn_notes_click(object sender, ImageClickEventArgs e)
    //{
    //   // Session["add_order_notes"] = "add";
    //    Response.Redirect("toporder_list_notes.aspx");
    //}
   
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
}
